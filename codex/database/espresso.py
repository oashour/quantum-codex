"""
Module for dealing with the Quantum ESPRESSO database
"""

import os
import shutil
import re
import json
from io import StringIO
import xml.etree.ElementTree as ET
import logging
from importlib import resources
import glob

from lxml.html import parse

from codex.utils import run_command, tidy_dict, tidy_str
from codex.database.utils import standardize_type

log = logging.getLogger(__name__)


def _parse_vargroup(vg):
    vars = []
    names = []
    type = vg.attrib["type"]
    info = vg.find("info")
    if info is not None:
        info = info.text

    for v in vg.findall("var"):
        v_dict = {
            "datatype": type,
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

    type = v.attrib.get("type", "Unknown")
    dim = v.attrib.get("end", 1)
    v_dict = {
        "datatype": type,
        "dimension": dim,
        "default": default,
        "options": options,
        "info": info,
    }

    return v_dict, v.attrib["name"]


def _get_summary(info):
    """
    Gets a terrible summary from the info string
    """
    summary = info.split(".")[0]
    summary = summary.split("-")[0]
    summary = summary.split("(")[0]
    summary = summary.split("see")[0]
    summary = summary.split(":")[0]

    return summary


def _tidy_vars(vars_dict):
    tidy_vars_dict = {}
    for namelist, tags in vars_dict.items():
        namelist = namelist.lower()
        tidy_vars_dict[namelist] = {}
        for name, t in tags.items():
            type = standardize_type(t["datatype"])
            dimension = t["dimension"]
            options = tidy_dict(t["options"])
            default = tidy_str(t["default"])
            info = tidy_str(t["info"])

            # Special cases
            if name == "A":
                info = "a in angstrom"
            elif name == "B":
                info = "b in angstrom"
            elif name == "C":
                info = "c in angstrom"
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
                    ".TRUE.": None,
                    ".FALSE.": None,
                }

            summary = _get_summary(info)
            if dimension != 1:
                type += f"array ({dimension})"

            tidy_vars_dict[namelist][name] = {
                "datatype": type,
                "default": default,
                "options": options,
                "summary": summary,
                "id": None,
                "info": info,
            }

    return tidy_vars_dict


# TODO: don't get rid of HTML formatting
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


def _add_html_ids(vars, html_filename):
    """
    Adds Base64 encoded HTML extracted from the helpdoc-generated HTML to each variable in dict vars
    Mutating function (on vars)
    """
    with open(html_filename, "r") as f:
        root = parse(f).getroot()

    id_map = {}
    # Find all links with href = "#idm*", their text is the name
    links = root.xpath('//a[starts-with(@href, "#id")]')
    for a in links:
        # The split accounts for some array edge cases in old documentation
        # TODO: some old documentation has 1 or 2D arrays that are not multidimensional type
        # Variable name is along the lines of "name(n_x,n_y)" which leads to errors in matching
        # With HTML
        name = a.text.split("(")[0]
        if name.startswith("&"):
            name = name[1:]
        id = a.attrib["href"][1:]
        id_map.update({name: id})

    for tags in vars.values():
        for name, t in tags.items():
            t["id"] = id_map.get(name, "#")

    return vars


def generate_database(version):
    """
    Generates the database.json file for a specific version of Quantum ESPRESSO.
    Assumes that helpdoc has already been run for this version and that the
    XML and HTML is in the database_dir/qe-<version> directory.
    """
    base_db_dir = resources.files("codex.database")
    database_dir = os.path.join(base_db_dir, "espresso-helpdoc", version)

    logging.info("Generating JSON for espresso version " + version)

    # Pull the tags and their info from the helpdoc-generated XML
    files = glob.glob(os.path.join(database_dir, "*.xml"), recursive=True)
    vars = {}
    for xml_filename in files:
        package = os.path.basename(xml_filename).split(".xml")[0]
        logging.info(f"Processing: {xml_filename} ({package}.x)")
        vars[package] = _extract_vars(xml_filename)

    # Pull the HTML from the helpdoc-generated HTML
    files = glob.glob(os.path.join(database_dir, "*.html"), recursive=True)
    for html_filename in files:
        package = os.path.basename(html_filename).split(".html")[0]
        logging.info(f"Processing: {html_filename} ({package}.x)")
        vars[package] = _add_html_ids(vars[package], html_filename)
        with open(html_filename, "r") as f:
            vars[package]["doc"] = f.read()

    json_filename = os.path.join(base_db_dir, "json", "espresso-" + version + ".json")
    with open(json_filename, "w") as f:
        logging.info(f"Writing JSON to {json_filename}.")
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
    sparse_checkout_source = os.path.join(
        base_db_dir, "espresso-helpdoc", "helpdoc-sparse-checkout"
    )
    sparse_checkout_dest = os.path.join(".git", "info", "sparse-checkout")
    shutil.copy2(sparse_checkout_source, sparse_checkout_dest)
    run_command("git config core.sparseCheckout true")
    run_command("git checkout")

    # Find all .def files
    files = glob.glob(os.path.join("**", "*.def"), recursive=True)
    for def_file in files:
        dir = os.path.dirname(def_file)
        input_xx_xsl = os.path.join(base_db_dir, "espresso-helpdoc", "input_xx.xsl")
        shutil.copy2(input_xx_xsl, dir)

    # So that this function doesn't change cwd
    os.chdir(root)
    files = [os.path.join(work_dir, "q-e", f) for f in files]

    return files


def run_helpdoc(version, no_cleanup=False):
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

    # Commands for picking the right versions
    devtools_dir = os.path.join(work_dir, "q-e", "dev-tools")
    database_dir = os.path.join(base_db_dir, "espresso-helpdoc", version)
    if not os.path.exists(database_dir):
        os.makedirs(database_dir)

    for def_file in files:
        package = os.path.basename(def_file).split("INPUT_")[-1]
        package = package.split(".")[0].lower()
        cmd_helpdoc = f"{devtools_dir}/helpdoc --version {version} {def_file}"
        run_command(cmd_helpdoc)

        # Copy the generated files to the database directory
        # Explicit destination is needed to overwrite existing files
        xml_file = os.path.splitext(def_file)[0] + ".xml"
        html_file = os.path.splitext(def_file)[0] + ".html"
        shutil.move(html_file, os.path.join(database_dir, f"{package}.html"))
        shutil.move(xml_file, os.path.join(database_dir, f"{package}.xml"))
    os.chdir(root)

    # Clean up
    log.info("Cleaning up helpdoc environment in " + work_dir)
    if not no_cleanup and os.path.exists(work_dir):
        shutil.rmtree(work_dir)
