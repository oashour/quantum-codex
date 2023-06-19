"""
Module for the Codex class, which is used to generate a codex from a DFT input
"""

import json
from base64 import b64decode
import os
from urllib.parse import quote
import textwrap
import shutil
from string import Template
from inspect import cleandoc
from importlib import resources
import logging
import random
import re

import f90nml
from pymatgen.io.vasp.inputs import Incar, Poscar, Potcar, Kpoints
from lxml import etree
from tabulate import tabulate

from codex.utils import range_dict_get, _format_value

WIKI_URL = "https://www.vasp.at/wiki/index.php"


def remove_html_tags(text):
    """Remove html tags from a string"""
    if text:
        parser = etree.HTMLParser()
        tree = etree.fromstring(text, parser)
        string = etree.tostring(tree, encoding="unicode", method="text")
        return string.strip()
    return text


class Codex:
    """
    A class to store the parsed information from a DFT input file
    """

    def __init__(self, input_filename, code, dbversion):
        self.base_db_dir = resources.files("codex.database")
        # Make all paths absolute
        self.working_dir = ".codex"  # TODO: remove
        self.dbversion = dbversion
        self.filename = os.path.basename(input_filename)

        # TODO: need some switch for determining whether QE or VASP
        if code == "Quantum ESPRESSO":
            self.code = "qe"
        elif code == "VASP":
            self.code = "vasp"
        else:
            self.code = code

        # TODO: rework entire database
        self.database_dir = os.path.join(self.base_db_dir, f"{self.code}-{dbversion}")
        self.database_filename = os.path.join(self.database_dir, "database.json")

        # Load Database
        with open(self.database_filename) as f:
            self._database = json.load(f)
        self.package = self.get_packages(input_filename)

        if self.code == "qe":
            # for file_id, file in enumerate(input_filenames):
            file = os.path.abspath(input_filename)
            tags, cards = self._get_qe_html(file)
        elif self.code == "vasp":
            file = os.path.abspath(input_filename)
            tags, cards = self._get_vasp_html(file)
        else:
            raise ValueError(f"Code {self.code} not recognized.")

        self.tags = tags
        self.cards = cards
        self.fileid = random.randint(0, 1000000)
        if self.code == "vasp":
            self.indent = 0 * " "
            self.comment_token = "! "
            self.section_start_token = ""
            self.section_end_token = ""
        elif self.code == "qe":
            self.indent = 2 * " "
            self.comment_token = "! "
            self.section_start_token = "&"
            self.section_end_token = "/"

    # TODO: abstract to VASP
    # TODO: these errors need to go to the webspage?
    def get_packages(self, filename):
        """
        This function finds what package a QE input file is for.
        Given the database, it flattens the dictionaries and
        the namelists and returns the list of packages that match a given file.
        """
        database = self._database

        if self.code == "qe":
            flat_db = self._flatten_dict(database)
            # for file in filenames:
            input_data = f90nml.read(filename)
            flat_input = self._flatten_dict({"_": input_data})["_"]
            matches = []
            for package, mashed_tags in flat_db.items():
                if all(tag in mashed_tags for tag in flat_input):
                    matches.append(package)
            if len(matches) == 0:
                raise ValueError(f"Could not find package for {filename}")
            if len(matches) > 1:
                logging.warning(f"Found multiple possible packages for {filename}: {matches}.")
                logging.warning("Using first package.")
            package = matches[0]
            return package
        if self.code == "vasp":
            basename = os.path.basename(filename)
            match = re.match(r"^(INCAR|POSCAR|KPOINTS|POTCAR)", basename)
            if match is None:
                raise ValueError(
                    f"Could not find package for {basename}. I can only read INCAR, POSCAR,"
                    "KPOINTS, and POTCAR files, and their names need to start that way,"
                    "e.g., INCAR, INCAR-Si, INCARSi, INCAR_Si, etc. are all valid"
                )
            package = match.group(1)
            return package

        raise ValueError(f"Code {self.code} not recognized.")

    def _get_pad_and_format(self, file_string):
        """
        Adjusts the spacing for the INCAR file
        """
        incar_string = file_string
        lines = []
        for l in incar_string.split("\n"):
            if l:
                tag = l.split("=")[0].strip()
                val = l.split("=")[1].strip()
                val = self._format_value(tag, val)
                comment = "! Place holder"
                lines.append([tag, "=", val, comment])
        table = tabulate(lines, tablefmt="plain")
        tags = {}
        for l in table.split("\n"):
            tag = l.split("=")[0]
            val = l.split("=")[1].split("!")[0]
            # The -1 accounts for the extra spacing around the = sign
            # that's being added by tabulate
            key = re.sub(r"\(.*\)", "", tag.strip())
            tags[key] = {
                "tag_pad": (len(tag) - len(tag.rstrip()) - 1) * " ",
                "value_pad": (len(val) - len(val.lstrip()) - 1) * " ",
                "comment_pad": (len(val) - len(val.rstrip()) - 1) * " ",
                "formatted_value": val.strip(),
                "formatted_tag": tag.strip(),
            }
        return tags

    def _format_value(self, tag, value):
        if self.code == "vasp":
            incar_string = f"{tag} = {value}"
            incar = Incar.from_string(incar_string)
            formatted_value = incar.get_string(sort_keys=True).split("=")[1].strip()
            formatted_value = formatted_value.replace("True", ".TRUE.")
            formatted_value = formatted_value.replace("False", ".FALSE.")
            return formatted_value
        if self.code == "qe":
            return value
        raise ValueError(f"Code {self.code} not recognized.")

    def _get_incar_tags(self, incar):
        database = self._database["INCAR"]
        spaced_incar = self._get_pad_and_format(incar.get_string())
        tag_list = []
        for tag, value in incar.items():
            comment = self._get_comment(tag, value, database)
            href = WIKI_URL + "/" + quote(tag)
            tag_list.append(
                {
                    "name": spaced_incar[tag]["formatted_tag"],
                    "value": spaced_incar[tag]["formatted_value"],
                    "comment": comment,
                    "tag_pad": spaced_incar[tag]["tag_pad"],
                    "value_pad": spaced_incar[tag]["value_pad"],
                    "comment_pad": spaced_incar[tag]["comment_pad"],
                    "id": tag,
                    "href": href,
                }
            )

        return tag_list

    def _get_vasp_html(self, input_filename):
        """
        Builds a codex for Quantum Espresso, returning HTML
        """
        if self.package == "INCAR":
            incar = Incar.from_file(input_filename)
            # This avoids some bugs
            incar = Incar({k.upper(): v for k, v in incar.items()})
            tags = self._get_incar_tags(incar)
            tags = {"": tags}
            cards = ""
        elif self.package == "POSCAR":
            # TODO: add POSCAR support
            poscar = Poscar.from_file(input_filename)
            tags = {}
            cards = poscar.get_string()
        elif self.package == "KPOINTS":
            # TODO: add KPOINTS support
            kpoints = Kpoints.from_file(input_filename)
            tags = {}
            cards = str(kpoints)
        elif self.package == "POTCAR":
            # TODO: add POTCAR support
            potcar = Potcar.from_file(input_filename)
            tags = {}
            cards = str(potcar)

        return tags, cards

    def _get_qe_html(self, input_filename):
        """
        Builds a codex for Quantum Espresso, returning HTML
        """
        # Parse namelists and cards
        input_file = f90nml.read(input_filename)
        with open(input_filename, "r") as f:
            cards = f.read().split("/")[-1].strip()

        tags_dict = self._get_qe_tags(input_file)

        return tags_dict, cards

    # TODO: this needs a lot of improvement
    @staticmethod
    def _get_comment(tag, val, database):
        if database[tag]["options"]:
            options = database[tag]["options"]
            comment = range_dict_get(str(val), options)
            if comment is not None:
                return remove_html_tags(comment)
        if database[tag]["summary"]:
            comment = remove_html_tags(database[tag]["summary"])
            if comment.startswith(tag):
                # TODO: this is vasp specific
                comment = comment.strip(tag).strip(", ").strip()
            return comment
        return "No Comment Available"

    @staticmethod
    def _nl_to_str(nl):
        """
        Converts a namelist from f90nml to a
        cleanly formatted string for use with
        _get_pad_and_format
        """
        nl_str = str(nl)
        # Strip all lines starting with & or /
        nl_str = "\n".join(
            [
                line
                for line in nl_str.split("\n")
                if not line.startswith("&") and not line.startswith("/") and line.strip()
            ]
        )
        # Replace all .true. with .TRUE. and .false. with .FALSE.
        nl_str = nl_str.replace(".true.", ".TRUE.").replace(".false.", ".FALSE.")
        return cleandoc(nl_str)

    def _get_qe_tags(self, input_file):
        database = self._database[self.package]
        pads_and_formats = self._get_pad_and_format(self._nl_to_str(input_file))
        tags_dict = {}
        for nl, namelist in input_file.items():
            tags_dict[nl] = []
            for tag, val in namelist.items():
                comment = self._get_comment(tag, val, database[nl])
                href = (
                    f"https://www.quantum-espresso.org/Doc/INPUT_{self.package.upper()}.html#{tag}"
                )
                # href = f"{self.package}.html#" + database[nl][tag]["id"]
                # TODO: doesn't work with lists with multiple elements
                tags_dict[nl].append(
                    {
                        "name": pads_and_formats[tag]["formatted_tag"],
                        "value": pads_and_formats[tag]["formatted_value"],
                        "comment": comment,
                        "tag_pad": pads_and_formats[tag]["tag_pad"],
                        "value_pad": pads_and_formats[tag]["value_pad"],
                        "comment_pad": pads_and_formats[tag]["comment_pad"],
                        "id": tag,
                        "href": href,
                    }
                )
        return tags_dict

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
