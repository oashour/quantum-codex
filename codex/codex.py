"""
Module for the Codex class, which is used to generate a codex from a DFT input
"""

import json
from base64 import b64decode
import os
import html
import textwrap
import shutil
from string import Template
from inspect import cleandoc
from importlib import resources
import logging

import f90nml

from codex.utils import range_dict_get, _nl_print


# TODO: use local jquery
CODEX_HTML = Template(
    """<!DOCTYPE html>
<html>
<head>
$indent<link rel="stylesheet" type="text/css" href="style.css"></link>
$indent<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.6.4/jquery.min.js"></script>
</head>
<body>
$body
<script src="script.js"></script>
</body>
</html>
"""
)
INDENT = 2


class Codex:
    """
    A class to store the parsed information from a DFT input file
    """

    def __init__(self, input_filenames, dbversion):
        self.base_db_dir = resources.files("codex.database")
        # Make all paths absolute
        self.working_dir = ".codex"
        self.dbversion = dbversion

        # TODO: need some switch for determining whether QE or VASP
        self.code = "qe"  # Need better terminology

        self.database_dir = os.path.join(self.base_db_dir, f"{self.code}-{dbversion}")
        self.database_filename = os.path.join(self.database_dir, "database.json")

        # Load Database
        with open(self.database_filename) as f:
            self._database = json.load(f)
        self.packages = self.get_packages(input_filenames)

        body_html = []
        if self.code == "qe":
            for file_id, file in enumerate(input_filenames):
                file = os.path.abspath(file)
                body_html.append(self._get_qe_html(file, file_id))
        elif self.code == "vasp":
            for file_id, file in enumerate(input_filenames):
                file = os.path.abspath(file)
                body_html.append(self._get_vasp_html(file, file_id))
        else:
            raise ValueError(f"Code {self.code} not recognized.")

        body_html = "\n".join(body_html)
        self.html = CODEX_HTML.substitute(body=body_html, indent=" " * INDENT)

    def build(self, filename="index.html"):
        """
        Build the codex, generating the HTML and copying the necessary files
        """
        self._generate_dir_tree()
        self._generate_tag_webpages()
        path = os.path.join(self.working_dir, filename)
        with open(path, "w") as f:
            f.write(self.html)

    # TODO: abstract to VASP
    def get_packages(self, filenames):
        """
        This function finds what package a QE input file is for.
        Given the database, it flattens the dictionaries and
        the namelists and returns the list of packages that match a given file.
        """
        database = self._database

        flat_db = self._flatten_dict(database)
        packages = []
        for file in filenames:
            input_data = f90nml.read(file)
            flat_input = self._flatten_dict({"_": input_data})["_"]
            matches = []
            for package, mashed_tags in flat_db.items():
                if all(tag in mashed_tags for tag in flat_input):
                    matches.append(package)
            if len(matches) == 0:
                raise ValueError(f"Could not find package for {file}")
            if len(matches) > 1:
                logging.warning(f"Found multiple possible packages for {file}: {matches}.")
                logging.warning("Using first package.")
            packages.append(matches[0])

        return packages


    def _generate_dir_tree(self):
        """
        Copies appropriate files from the database to the working directory
        and creates the tree
        """
        if os.path.exists(self.working_dir):
            shutil.rmtree(self.working_dir)
        os.mkdir(self.working_dir)
        tags_dir = os.path.join(self.working_dir, f"tags-{self.code}")
        os.mkdir(tags_dir)

        style_css = os.path.join(self.base_db_dir, "style.css")
        tags_css = os.path.join(self.base_db_dir, f"tag-{self.code}.css")
        script_js = os.path.join(self.base_db_dir, "script.js")
        docs_html = []
        for p in self.packages:
            docs_html.append(os.path.join(self.database_dir, f"{p}.html"))

        shutil.copy2(style_css, self.working_dir)
        shutil.copy2(tags_css, self.working_dir)
        shutil.copy2(script_js, self.working_dir)
        for doc in docs_html:
            shutil.copy2(doc, self.working_dir)

    def _get_qe_html(self, input_filename, file_id):
        """
        Builds a codex for Quantum Espresso, returning HTML
        """
        # Parse namelists and cards
        input_file = f90nml.read(input_filename)
        with open(input_filename, "r") as f:
            cards = f.read().split("/")[-1].strip()

        comment_indents = self._find_comment_indents(input_file)

        tag_html = '<div class="row">\n<div class="column left monospace">'
        preview_html = '<div class="cloumn right">\n'
        for nl in input_file.keys():
            tag_html += html.escape(f"&{nl.upper()}\n")
            namelist = input_file[nl]
            for tag, val in namelist.items():
                tag_html += self._get_tag_html(nl, tag, val, comment_indents, file_id)
                preview_html += self._get_preview_html(tag, nl, file_id)

            tag_html += f"/\n"
        tag_html += cards + "\n</div>\n"
        preview_html += "</div>"

        filename = os.path.basename(input_filename)
        webpage = cleandoc(
            f"""
        <div class="input-file">
        <h2>{filename}</h2>
        Detected code <b>Quantum Espresso</b> (package: {self.packages[file_id]}.x)
        <!-- Input File -->
        {tag_html}
        <!-- Previews -->
        {preview_html}
        </div>
        """
        )

        return webpage

    def _generate_tag_webpages(self):
        database = self._database
        tags_dir = os.path.join(self.working_dir, "tags-qe")
        packages = set(self.packages)
        for p in packages:
            os.mkdir(os.path.join(tags_dir, p))
            for nl in database[p].keys():
                os.mkdir(os.path.join(tags_dir, p, nl))
                for tag in database[p][nl].keys():
                    v = database[p][nl][tag]
                    if v["html"] != "":
                        webpage = b64decode(v["html"]).decode("utf-8")
                        path = os.path.join(tags_dir, p, nl, f"{tag}.html")
                        with open(path, "w") as f:
                            f.write(webpage)

    @staticmethod
    def _find_comment_indents(namelists):
        """
        Computes how much to indent each comment based on the length of its tag
        so that all comments in the file are aligned.
        Returns dictionary of {tag: indent}, where if tag is an array then
        indent is a list of indents for each element of the array.
        """
        tag_lengths = {}  # Dict of {tag: length}
        for nl in namelists.keys():
            for tag, val in namelists[nl].items():
                if isinstance(val, list):
                    array_tag_len = []
                    for i, v in enumerate(val):
                        string = f"{tag}({i+1}) = {_nl_print(val[i])}"
                        array_tag_len.append(len(string))
                    tag_lengths.update({tag: array_tag_len})
                else:
                    string = f"{tag} = {_nl_print(val)}"
                    tag_lengths.update({tag: len(string)})

        # Figure out maximum length of all lines
        all_lengths = []
        for k, v in tag_lengths.items():
            all_lengths.extend([v] if isinstance(v, int) else v)
        max_length = max(all_lengths)

        # Compute indents based on longest line
        indents = {}
        for k, v in tag_lengths.items():
            if isinstance(v, list):
                indents[k] = [(max_length - i) for i in v]
            else:
                indents[k] = max_length - v

        return indents

    # TODO: this needs a lot of improvement
    @staticmethod
    def _get_comment(tag, val, database):
        if database[tag]["options"]:
            options = database[tag]["options"]
            comment = range_dict_get(str(val), options)
            if comment is None:
                comment = "Unknown value, check documentation."
        elif database[tag]["info"]:
            comment = database[tag]["info"]
        else:
            comment = ""
        comment = ' <span class="comment">! ' + comment if comment else ""
        comment = comment.split(".")[0]
        comment = comment.split("-")[0]
        comment = comment.split("(")[0]
        comment = comment.split("see")[0]
        comment = comment.split(":")[0]
        comment += "</span>" if comment else ""

        return comment

    def _get_tag_html(self, namelist, tag, val, comment_indents, file_id):
        package = self.packages[file_id]
        database = self._database[package][namelist]
        if self.code == "qe":
            link = f"{package}.html#" + database[tag]["id"]
        elif self.code == "vasp":
            link = "https://www.vasp.at/wiki/index.php/" + tag

        link = f'<a href="{link}" class = "tag-link" id="{tag}" data-fileid="{file_id}">{tag}</a>'


        tag_link = ""
        # Array variables require different printing
        # TODO: abstract this to work for VASP?
        if isinstance(val, list):
            for i, v in enumerate(val):
                comment = self._get_comment(tag, v, database)
                cind = comment_indents[tag][i] * " "
                tag_link += f"{link}({i+1}) = {_nl_print(v)}{cind}{comment}\n"
        else:
            comment = self._get_comment(tag, val, database)
            cind = comment_indents[tag] * " "
            tag_link += f"{link} = {_nl_print(val)}{cind}{comment}\n"

        return textwrap.indent(tag_link, " " * INDENT)

    def _get_preview_html(self, tag, namelist, file_id):
        p = self.packages[file_id]
        nl = namelist
        preview_link = os.path.join(f"tags-{self.code}", p, nl, f"{tag}.html")
        preview = f'<div class="preview" id="preview_{tag}" data-fileid="{file_id}">\n'
        preview += f'<object data="{preview_link}" class="preview-object" type="text/html">'
        preview += " </object>\n</div>\n"
        return textwrap.indent(preview, " " * INDENT)

    @staticmethod
    def _flatten_dict(d):
        """
        Takes a dict of dicts of dicts and flattens it into a dict of lists
        e.g.,
        {'a': {'b': {'c': 1, 'd': 2}, 'e': {'f': 3, 'g': 4}}}
        becomes
        {'a': ['b.c', 'b.d', 'e.f', 'e.g']}
        """
        flat = {}
        for top_key, middle_dict in d.items():
            flat[top_key] = []
            for middle_key, bottom_dict in middle_dict.items():
                for bottom_key in bottom_dict.keys():
                    flat[top_key].append(middle_key + "." + bottom_key)
        return flat
