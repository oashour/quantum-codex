
import json
from base64 import b64decode
import f90nml
import os
import html
from textwrap import indent
import shutil

def _nl_print(string):
    if string is True:
        return '.true.'
    elif string is False:
        return '.false.'
    elif isinstance(string, str):
        return f"'{string}'"
    else:
        return string

class Codex:
    """
    A class to store the parsed information from a DFT input file
    """
    def __init__(self, input_filename, database_dir, version):

        # Make all paths absolute
        self.root = os.getcwd() 
        self.working_dir = os.path.join(self.root, '.codex') # Codex working directory
        self.input_filename = os.path.abspath(input_filename)
        self.database_dir = os.path.abspath(database_dir)
        self.database_filename = os.path.join(database_dir, 'qe-'+version, 'database.json')

        # Load Database
        with open(self.database_filename) as f:
            database = json.load(f)

        self._generate_tag_webpages(database)
        self._build_qe_codex(input_filename, database)

        style_css = os.path.join(self.database_dir, 'style.css')
        tags_css = os.path.join(self.database_dir, 'qe-tag.css')
        script_js = os.path.join(self.database_dir, 'script.js')
        docs_html = os.path.join(self.database_dir, 'qe-'+version, 'INPUT_PW.html')
        if not os.path.exists(self.working_dir):
            os.mkdir(self.working_dir)
        tags_dir = os.path.join(self.working_dir, 'tags')
        if not os.path.exists(tags_dir):
            os.mkdir(tags_dir)
        shutil.copy2(style_css, self.working_dir)
        shutil.copy2(tags_css, self.working_dir)
        shutil.copy2(docs_html, self.working_dir)
        shutil.copy2(script_js, self.working_dir)

    def _build_qe_codex(self, input_filename, database):
        working_dir = self.working_dir
        ind=2 # Indentation level
        # Parse namelists
        input_file = f90nml.read(input_filename)
        # Parse cards
        with open(input_filename, 'r') as f:
            cards = f.read().split('/')[-1]
        
        comment_indents = self._find_comment_indents(input_file)

        #base_link = "file:///" + os.path.join(working_dir, "INPUT_PW.html")

        tag_html = '<div class="row">\n<div class="column left">\n<div class="input-file">\n'
        preview_html = '<div class="cloumn right">\n'
        for nl in input_file.keys():
            tag_html += indent(html.escape(f"&{nl}\n"), ' '*ind)
            namelist = input_file[nl]
            for tag, val in namelist.items():
                idm = database[nl][tag]['idm']
                link = "INPUT_PW.html#"+idm
                link = f'<a href="{link}" class = "tag-link" id="{tag}">{tag}</a>'

                if database[nl][tag]['options']:
                    options = database[nl][tag]['options']
                    comment = options.get(str(val), None)
                    if comment is None:
                        comment = "Unknown Value"
                elif database[nl][tag]['info']:
                    comment = database[nl][tag]['info']
                else:
                    comment = ''
                comment = ' <span class="comment">! '+comment if comment else ''
                comment = comment.split('.')[0]
                comment = comment.split('-')[0]
                comment = comment.split('(')[0]
                comment = comment.split('see')[0]
                comment = comment.split(':')[0]
                comment += '</span>' if comment else ''

                tag_link = ''
                if isinstance(val, list):
                    for i, v in enumerate(val):
                        cind = comment_indents[tag][i] * ' '
                        tag_link += f"{link}({i+1}) = {_nl_print(v)}{cind}{comment}\n"
                else:
                    cind = comment_indents[tag] * ' '
                    tag_link += f"{link} = {_nl_print(val)}{cind}{comment}\n"
                tag_html += indent(tag_link, '  '*ind)

                #preview_link = "file:///" + os.path.join(working_dir, "tags", f"{tag}.html")
                #preview_link = "file:///" + os.path.join("tags", f"{tag}.html")
                preview_link = os.path.join("tags", f"{tag}.html")
                preview = f'<div class="preview" id="preview_{tag}">\n'
                preview += f'<object data="{preview_link}" class="preview-object" type="text/html">'
                preview += ' </object>\n</div>\n'
                preview_html += indent(preview, ' '*ind)

            tag_html += f"{' '*ind}/\n"
        preview_html += "</div>"
        tag_html += cards
        tag_html += "</div>\n</div>\n"
        webpage_html = tag_html + preview_html

        # TODO: use local jquery
        webpage = f"""<!DOCTYPE html>
        <html>
        <head>
        {' '*ind}<link rel="stylesheet" type="text/css" href="style.css"></link>
        {' '*ind}<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.6.4/jquery.min.js"></script>
        </head>
        <body>
        <h2>Input File</h2>
        The following information was parsed from <tt>{input_filename}</tt>:

        {webpage_html}
        <script src="script.js"></script>
        </body>
        </html>
        """
        path = os.path.join(working_dir, 'codex.html')
        with open(path, 'w') as f:
            f.write(webpage)

    def _generate_tag_webpages(self, database):
        tags_dir = os.path.join(self.working_dir, 'tags')
        for nl in database.keys():
            for tag in database[nl].keys():
                v = database[nl][tag]
                if v["html"] != "":
                    webpage = b64decode(v["html"]).decode("utf-8")
                    path = os.path.join(tags_dir, f'{tag}.html')
                    with open(path, "w") as f:
                        f.write(webpage)

    @staticmethod
    def _find_comment_indents(namelists):
        """
        Computes how much to indent each comment based on the length of the
        tag it describes so that all comments in the file are aligned.
        Returns dictionary of {tag: indent}, where if tag is an array then 
        indent is a list of indents for each element of the array.
        """
        tag_lengths = {} # Dict of {tag: length}
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
                indents[k] = (max_length - v)

        return indents
