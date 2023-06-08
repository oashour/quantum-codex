"""
Module for dealing with the Quantum ESPRESSO database
"""

import os
import shutil
import re
from base64 import b64encode
import json
from io import StringIO
import xml.etree.ElementTree as ET
import logging
from importlib import resources
import glob

from lxml.etree import tostring
from lxml.html import soupparser

from codex.utils import run_command, tidy_dict, tidy_str, wipe_style

log = logging.getLogger(__name__)

# We normalize both VASP and QE data types to same naming convention (i.e., python types)
type_map = {
    "character": "str",
    "string": "str",
    "real": "float",
    "double": "float",
    "integer": "int",
    "logical": "bool",
    "structure": "dict",
    "unknown": "unknown",
}


def _parse_vargroup(vg):
    vars = []
    names = []
    type = vg.attrib["type"]
    info = vg.find("info")
    if info is not None:
        info = info.text

    for v in vg.findall("var"):
        v_dict = {
            # "name": v.attrib["name"], # Only here temporarily
            "type": type,
            "info": info,
            "dimension": 1,
            "default": " ",
            "options": {},
        }
        vars.append(v_dict)
        names.append(v.attrib["name"])

    return vars, names


def _parse_group(g):
    vars = []
    names = []
    for e in g:
        if e.tag in ("var", "multidimension", "dimension"):
            v, n = _parse_var(e)
            vars.append(v)
            names.append(n)
        elif e.tag == "vargroup":
            v, n = _parse_vargroup(e)
            vars.extend(v)
            names.extend(n)
        elif e.tag == "group":
            v, n = _parse_group(e)
            vars.extend(v)
            names.extend(n)
    return vars, names


def _parse_var(v):
    opts = v.find("options")

    # Deal with Info
    info = None
    if opts is not None:
        info = opts.find("info")
    else:
        info = v.find("info")
    if info is not None:
        info = info.text
    else:
        info = ""

    options = {}
    if opts is not None:
        for o in opts.findall("opt"):
            options.update({o.attrib["val"]: o.text})

    default = v.find("default")
    if default is not None:
        default = default.text
    else:
        default = ""

    v_dict = {
        # "name": v.attrib["name"], # Keep here temporarily
        "type": v.attrib.get("type", "UNKNOWN"),
        "dimension": v.attrib.get("end", 1),
        "info": info,
        "default": default,
        "options": options,
    }

    return v_dict, v.attrib["name"]


def _tidy_vars(vars_dict):
    # vars is now a dict of dicts
    tidy_vars_dict = {}
    for namelist, tags in vars_dict.items():
        namelist = namelist.lower()
        tidy_vars_dict[namelist] = {}
        for name, t in tags.items():
            type = type_map[t["type"].lower()]
            dimension = t["dimension"]
            options = tidy_dict(t["options"])
            default = tidy_str(t["default"])
            info = tidy_str(t["info"])

            # Special cases
            if name == "A":
                info = "a in ANGSTROM"
            elif name == "B":
                info = "b in ANGSTROM"
            elif name == "C":
                info = "c in ANGSTROM"
            elif name == "cosAB":
                info = "cos angle between a and b (gamma)"
            elif name == "cosAB":
                info = "cos angle  between a and c (beta)"
            elif name == "cosBC":
                info = "cos angle between b and c (alpha)"
            elif name == "ibrav":
                info = "Bravais lattice choice"
                options = {
                    0: "Lattice in CELL_PARAMETERS",
                    1: "Cubic P (sc) lattice",
                    2: "Cubic F (fcc) lattice",
                    3: "Cubic I (bcc) lattice",
                    -3: "Cubic I (bcc) lattice",
                    4: "Hexagonal and Trigonal P lattice",
                    5: "Trigonal Rhombohedral lattice, 3-fold axis c",
                    -5: "Trigonal Rhombohedral lattice, 3-fold axis <111>",
                    6: "Tetragonal P (st) lattice",
                    7: "Tetragonal I (bct) lattice",
                    8: "Orthorhombic P lattice",
                    9: "Orthorhombic base-centered(bco) lattice",
                    -9: "Orthorhombic base-centered(bco) lattice",
                    91: "Orthorhombic one-face base-centered A-type lattice",
                    10: "Orthorhombic face-centered lattice",
                    11: "Orthorhombic body-centered lattice",
                    12: "Monoclinic P, unique axis c lattice",
                    -12: "Monoclinic P, unique axis b lattice",
                    13: "Monoclinic base-centered lattice",
                    -13: "Monoclinic base-centered lattice",
                    14: "Triclinic lattice",
                }
            # TODO: implement parsing for these
            if type == "bool" and not options:
                options = {
                    "True": "...[parsing not implemented]",
                    "False": "...[parsing not implemented]",
                }

            tidy_vars_dict[namelist][name] = {
                "name": name,  # Keep here temporarily
                "type": type,
                "dimension": dimension,
                "options": options,
                "default": default,
                "info": info,
            }

    return tidy_vars_dict


def _extract_vars(xml_filename):
    pattern = re.compile(r'<a href="(.*?)">\s*(.*?)\s*</a>')
    with open(xml_filename, "r") as f:
        xmltext = f.read()
        xmltext = xmltext.replace("<ref>", "")
        xmltext = xmltext.replace("</ref>", "")
        xmltext = xmltext.replace("<b>", "")
        xmltext = xmltext.replace("</b>", "")
        xmltext = pattern.sub(r"\2 (\1)", xmltext)
        try:
            root = ET.parse(StringIO(xmltext)).getroot()
        except ET.ParseError:
            print(f"Error parsing xml file: {xml_filename}")
            raise

    vars = {}
    # cards = []
    for child in root:
        if child.tag == "namelist":
            namelist_name = child.attrib["name"]
            vars.update({namelist_name: {}})
            for e in child:
                if e.tag in ("var", "multidimension", "dimension"):
                    v, n = _parse_var(e)
                    vars[namelist_name].update({n: v})
                elif e.tag == "vargroup":
                    v_list, n_list = _parse_vargroup(e)
                    for v, n in zip(v_list, n_list):
                        vars[namelist_name].update({n: v})
                elif e.tag == "group":
                    v_list, n_list = _parse_group(e)
                    for v, n in zip(v_list, n_list):
                        vars[namelist_name].update({n: v})
        # elif child.tag == 'card':
        #    cards.append(child)

    vars = _tidy_vars(vars)

    return vars


def _generate_id_map(soup):
    """
    Generates a map from tag name -> {id, html}
    """
    id_map = {}
    # Find all links with href = "#idm*", their text is the name
    links = soup.xpath('//a[starts-with(@href, "#id")]')
    for a in links:
        # The split accounts for some array edge cases in old documentation
        # TODO: some old documentation has 1 or 2D arrays that are not multidimensional type
        # Variable name is along the lines of "name(n_x,n_y)" which leads to errors in matching
        # With HTML
        name = a.text.split("(")[0]
        # name = a.text
        if name.startswith("&"):
            name = name[1:]
        id = a.attrib["href"][1:]
        id_map.update({name: {"id": id, "html": None}})
    # Find all a tags with name="name", the table is an ancestor
    for name, id_dict in id_map.items():
        tags = soup.xpath(f'//a[@name="{name}"]')
        for a in tags:
            html = None
            # This accounts for most cases
            for sibling in a.itersiblings():
                if sibling.tag == "table":
                    html = sibling
                    break
            # This accounts for stuff in groups
            if html is None:
                for parent in a.iterancestors():
                    if parent.tag == "table":
                        html = parent
                        break
            id_dict["html"] = html

    return id_map


def _generate_tag_html(html):
    if html.tag == "table":
        html.classes.add("tag-table")
        wipe_style(html)
        tags = html.xpath(f"//th")
        for tag in tags:
            tag.classes.add("header-cell")
            wipe_style(tag)
        tags = html.xpath(f"//td")
        for child in tags:
            if "style" in child.attrib:
                style = child.attrib["style"].split(";")
                style = [s for s in style if s.strip()]
                style = dict(s.split(":") for s in style)
                style = {k.strip(): v.strip() for k, v in style.items()}
                if "background" in style and "text-align" in style:
                    bgcol = style["background"]
                    align = style["text-align"]
                    if bgcol == "#ffffc3" and align == "left":
                        child.classes.add("type-cell")
                    elif bgcol == "#ffffc3" and align == "right":
                        child.classes.add("datalabel-cell")
                    elif bgcol == "#fff3d9" and align == "left":
                        child.classes.add("data-cell")
            elif "colspan" in child.attrib and child.attrib["colspan"] == "2":
                child.classes.add("description-cell")
            wipe_style(child)
        tags = html.xpath(f"//pre")
        for child in tags:
            # child.string = tidy_str(child.text)
            # print('found pre')
            # Find if it has a <a> tag with href = "#*", if so, replace with <tt>
            child.tag = "p"
            links = child.xpath('//a[starts-with(@href, "#")]')
            for a in links:
                a.classes.add("tag-link")
                a.attrib["href"] = a.attrib["href"][1:] + ".html"

            # TODO: for ibrav, create a clean string and
            # use that instead of the cleaning preocedure
            # You should hash the string in the documentation
            # And if it doesn't match use the unforomatted one from documentation
            # TODO: You end up with a blob of text and no parageaphs. Hard to read.
            wipe_style(child)
            string = tostring(child, encoding="unicode")
            string = tidy_str(string)
            string = re.sub(r"\.TRUE\.", r"<tt>.TRUE.</tt>", string)
            string = re.sub(r"\.FALSE\.", r"<tt>.FALSE.</tt>", string)
            new_element = soupparser.fromstring(string)
            child.getparent().replace(child, new_element)

        # TODO: cleanup
        webpage_template = """
        <!DOCTYPE html>
        <html>
        <head>
            <link rel="stylesheet" type="text/css" href="../tag-qe.css">
        </head>
        <body>
        </body>
        </html>
        """
        webpage = soupparser.fromstring(webpage_template)
        body = webpage.xpath("//body")[0]
        body.append(html)

        return tostring(webpage, encoding="unicode", pretty_print=True)


def _add_html_info(vars, html_filename):
    """
    Adds Base64 encoded HTML extracted from the helpdoc-generated HTML to each variable in dict vars
    Mutating function (on vars)
    """
    # TODO: port away from soup parser, the HTML is clean
    with open(html_filename, "r") as f:
        soup = soupparser.fromstring(f.read())

    id_map = _generate_id_map(soup)
    for namelist, tags in vars.items():
        for name, t in tags.items():
            t["html"] = "No documentation was found for this tag."
            if name in id_map:
                t["id"] = id_map[name]["id"]
                if id_map[name]["html"] is not None:
                    tag_html = _generate_tag_html(id_map[name]["html"])
                    t["html"] = b64encode(tag_html.encode("utf-8")).decode("utf-8")
            else:
                logging.warning(f"WARNING: No HTML found for {name}")
    return vars


def generate_database(version):
    """
    Generates the database.json file for a specific version of Quantum ESPRESSO.
    Assumes that helpdoc has already been run for this version and that the
    XML and HTML is in the database_dir/qe-<version> directory.
    """
    base_db_dir = resources.files("codex.database")
    database_dir = os.path.join(base_db_dir, "qe-" + version)

    logging.info("Generating database for QE version " + version)

    # Pull the tags and their info from the helpdoc-generated XML
    files = glob.glob(os.path.join(database_dir, "*.xml"), recursive=True)
    vars = {}
    for xml_filename in files:
        code = os.path.basename(xml_filename).split("INPUT_")[-1]
        code = code.split(".")[0].lower()
        logging.info(f"Processing: {xml_filename} (code: {code}.x)")
        vars[code] = _extract_vars(xml_filename)

    # Pull the HTML from the helpdoc-generated HTML
    files = glob.glob(os.path.join(database_dir, "*.html"), recursive=True)
    for html_filename in files:
        code = os.path.basename(html_filename).split("INPUT_")[-1]
        code = code.split(".")[0].lower()
        logging.info(f"Processing: {html_filename} (code: {code}.x)")
        vars[code] = _add_html_info(vars[code], html_filename)

    json_filename = os.path.join(database_dir, "database.json")
    with open(json_filename, "w") as f:
        logging.info(f"Writing database to {json_filename}")
        json.dump(vars, f, indent=4)

    return vars


def _prepare_helpdoc_environment(work_dir, base_db_dir, version):
    root = os.getcwd()
    os.chdir(work_dir)
    # Delete the repo if it exists
    if os.path.exists("q-e"):
        shutil.rmtree("q-e")
    # 6.3, 6.5 and 6.7 have special tags
    # TODO: there's 6.3 and 6.3 MaX... need to handle this
    tag = "qe-" + version
    tag += "MaX" if version in ("6.3", "6.5") else ""
    tag += "MaX-Release" if version == "6.7" else ""

    # Clone essentially an empty repo with latest commit in tag's history
    cmd_clone = (
        "git clone --filter=blob:none --sparse --depth 1 --no-checkout "
        f"-b {tag} https://gitlab.com/QEF/q-e.git"
    )
    run_command(cmd_clone)
    os.chdir("q-e")

    # Checks out about 2 MB of files, the bare minimum to build the database
    sparse_checkout_source = os.path.join(base_db_dir, "qe-sparse-checkout")
    sparse_checkout_dest = os.path.join(".git", "info", "sparse-checkout")
    shutil.copy2(sparse_checkout_source, sparse_checkout_dest)
    run_command("git config core.sparseCheckout true")
    run_command("git checkout")

    # Find all .def files
    files = glob.glob(os.path.join("**", "*.def"), recursive=True)
    for def_file in files:
        dir = os.path.dirname(def_file)
        input_xx_xsl = os.path.join(base_db_dir, "input_xx.xsl")
        shutil.copy2(input_xx_xsl, dir)

    # So that this function doesn't change cwd
    os.chdir(root)
    files = [os.path.join(work_dir, "q-e", f) for f in files]

    return files


def run_helpdoc(version, no_cleanup=False):  # , base_db_dir=None):
    """
    Generates the help files for a specific version of Quantum ESPRESSO using helpdoc.
    This function does the following:
    1. Sparse clones the Quantum ESPRESSO repository from gitlab into work_dir/q-e
    2. Initializes the minimum number of files needed for helpdoc to work
    3. Runs helpdoc
    4. Copies the XML and HTML files to the database

    work_dir: temporary directory where repo is cloned and helpdoc run. Relative or absolute path.
    version: version of Quantum ESPRESSO to generate help files for (e.g. 6.3, not qe-6.3MaX)
    ### base_db_dir: base directory where the database is stored

    Requirements:
    git 2.37.1 or later (fairly modern version, make sure to update if needed)
    tcl, tcllib and xsltproc
    Can be installed via:
    ```
    $ apt-get install tcl tcllib xsltproc # Debian
    $ yum install tcl tcllib xsltproc # Red Hat
    $ brew install tcl-tk libxslt # Mac with Homebrew
    ```
    """
    base_db_dir = resources.files("codex.database")

    # Create work directory and go there
    root = os.getcwd()
    work_dir = ".codex_db"
    if not os.path.exists(work_dir):
        os.makedirs(work_dir)

    # Commands to set up minimal helpdoc environment
    log.info("Setting up helpdoc environment in " + work_dir)
    files = _prepare_helpdoc_environment(work_dir, base_db_dir, version)
    print(files)

    # Commands for picking the right versions
    devtools_dir = os.path.join(work_dir, "q-e", "dev-tools")
    database_dir = os.path.join(base_db_dir, "qe-" + version)
    if not os.path.exists(database_dir):
        os.makedirs(database_dir)

    for def_file in files:
        cmd_helpdoc = f"{devtools_dir}/helpdoc --version {version} {def_file}"
        run_command(cmd_helpdoc)

        # Copy the generated files to the database directory
        # Explicit destination is needed to overwrite existing files
        xml_file = os.path.splitext(def_file)[0] + ".xml"
        html_file = os.path.splitext(def_file)[0] + ".html"
        shutil.move(html_file, os.path.join(database_dir, os.path.basename(html_file)))
        shutil.move(xml_file, os.path.join(database_dir, os.path.basename(xml_file)))
    os.chdir(root)

    # Clean up
    log.info("Cleaning up helpdoc environment in " + work_dir)
    if not no_cleanup and os.path.exists(work_dir):
        shutil.rmtree(work_dir)
