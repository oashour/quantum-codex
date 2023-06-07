import os
import shutil
import re
from base64 import b64encode
import json
from io import StringIO

from lxml.etree import tostring
from lxml.html import soupparser
import xml.etree.ElementTree as ET

from codex.utils import run_command, tidy_dict, tidy_str, wipe_style

type_map = {
    "character": "str",
    "real": "float",
    "integer": "int",
    "logical": "bool",
    "unknown": "unknown",
}


def parse_vargroup(vg, parent):
    vars = []
    type = vg.attrib["type"]
    info = vg.find("info")
    if info is not None:
        info = info.text

    for v in vg.findall("var"):
        v_dict = {
            "name": v.attrib["name"],
            "parent": parent,
            "type": type,
            "info": info,
            "dimension": 1,
            "default": " ",
            "options": {},
        }
        vars.append(v_dict)

    return vars


def parse_group(g, parent):
    vars = []
    for e in g:
        if e.tag in ("var", "multidimension", "dimension"):
            vars.append(parse_var(e, parent))
        elif e.tag == "vargroup":
            vars.extend(parse_vargroup(e, parent))
        elif e.tag == "group":
            vars.extend(parse_group(e, parent))
    return vars


def parse_var(v, parent):
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
        "name": v.attrib["name"],
        "parent": parent,
        "type": v.attrib.get("type", "UNKNOWN"),
        "dimension": v.attrib.get("end", 1),
        "info": info,
        "default": default,
        "options": options,
    }

    return v_dict


def tidy_vars(vars):
    clean_vars = []
    for v in vars:
        name = v["name"]
        parent = v["parent"].lower()
        type = type_map[v["type"].lower()]
        dimension = v["dimension"]
        options = tidy_dict(v["options"])
        default = tidy_str(v["default"])
        info = tidy_str(v["info"])

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
        if type == "bool" and options == {}:
            options = {
                "True": "...[parsing not implemented]",
                "False": "...[parsing not implemented]",
            }

        clean_vars.append(
            {
                "name": name,
                "parent": parent,
                "type": type,
                "dimension": dimension,
                "options": options,
                "default": default,
                "info": info,
            }
        )

    return clean_vars


def extract_vars(xml_filename):
    pattern = re.compile(r'<a href="(.*?)">\s*(.*?)\s*</a>')
    with open(xml_filename, "r") as f:
        xmltext = f.read()
        xmltext = xmltext.replace("<ref>", "")
        xmltext = xmltext.replace("</ref>", "")
        xmltext = xmltext.replace("<b>", "")
        xmltext = xmltext.replace("</b>", "")
        xmltext = pattern.sub(r"\2 (\1)", xmltext)
        root = ET.parse(StringIO(xmltext)).getroot()

    vars = []
    # cards = []
    for child in root:
        if child.tag == "namelist":
            namelist_name = child.attrib["name"]
            for e in child:
                if e.tag in ("var", "multidimension", "dimension"):
                    vars.append(parse_var(e, namelist_name))
                elif e.tag == "vargroup":
                    vars.extend(parse_vargroup(e, namelist_name))
                elif e.tag == "group":
                    vars.extend(parse_group(e, namelist_name))
        # elif child.tag == 'card':
        #    cards.append(child)

    vars = tidy_vars(vars)

    return vars


# Generates a map from name -> {idm, html}
def generate_idm_map(soup):
    idm_map = {}
    # Find all links with href = "#idm*", their text is the name
    links = soup.xpath('//a[starts-with(@href, "#idm")]')
    for a in links:
        # The split accounts for some array edge cases in old documentation
        name = a.text.split("(")[0]
        if name.startswith("&"):
            name = name[1:]
        idm = a.attrib["href"][1:]
        idm_map.update({name: {"idm": idm, "html": ""}})
    # Find all a tags with name="name", the table is an ancestor
    for name, idm_dict in idm_map.items():
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
            idm_dict["html"] = html

    return idm_map


def generate_webpage(html):
    if isinstance(html, str):
        print(html)
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


def add_html_info(vars, html_filename):
    """
    Adds Base64 encoded HTML extracted from the helpdoc-generated HTML to each variable in vars
    """
    with open(html_filename, "r") as f:
        soup = soupparser.fromstring(f.read())

    idm_map = generate_idm_map(soup)
    for v in vars:
        name = v["name"]
        if name in idm_map:
            v["idm"] = idm_map[name]["idm"]
            if idm_map[name]["html"] is not None:
                webpage = generate_webpage(idm_map[name]["html"])
                v["html"] = b64encode(webpage.encode("utf-8")).decode("utf-8")
            else:
                v["html"] = ""
        else:
            print(f"WARNING: No HTML info for {name}")
    return vars


def generate_database(version, database_dir):
    """
    Generates the database.json file for a specific version of Quantum ESPRESSO.
    Assumes that helpdoc has already been run for this version and that the
    XML and HTML is in the database_dir/qe-<version> directory.
    """
    if isinstance(version, str):
        version = [version]
    for ver in version:
        xml_filename = os.path.join(database_dir, "qe-" + ver, "INPUT_PW.xml")
        vars = extract_vars(xml_filename)
        html_filename = os.path.join(database_dir, "qe-" + ver, "INPUT_PW.html")
        vars = add_html_info(vars, html_filename)
        parents = list(set([v["parent"] for v in vars]))
        vars_dict = {p: {} for p in parents}
        for v in vars:
            vars_dict[v["parent"]].update({v["name"]: v})
        json_filename = os.path.join(database_dir, "qe-" + ver, "database.json")
        with open(json_filename, "w") as f:
            json.dump(vars_dict, f, indent=4)


def run_helpdoc(work_dir, def_files, versions, base_db_dir):
    """
    Generates the help files for a specific version of Quantum ESPRESSO using helpdoc.
    This function does the following:
    1. Sparse clones the Quantum ESPRESSO repository from gitlab into work_dir/q-e
    2. Initializes the minimum number of files needed for helpdoc to work
    """
    # Minimal files needed for helpdoc to work
    helpdoc_files = [
        "dev-tools/helpdoc",
        "dev-tools/helpdoc.d",
        "dev-tools/helpdoc.schema",
        "dev-tools/input_xx.xsl",
        "GUI/Guib/lib",
    ]

    # Create work directory and go there
    root = os.getcwd()
    work_dir = os.path.join(root, work_dir)
    # TODO: this is temporary
    if os.path.exists(work_dir):
        shutil.rmtree(work_dir)
    if not os.path.exists(work_dir):
        os.makedirs(work_dir)
    os.chdir(work_dir)

    # Commands to set up minimal helpdoc environment
    qe_dir = os.path.join(work_dir, "q-e")
    cmd_clone = "git clone --filter=blob:none --sparse https://gitlab.com/QEF/q-e.git"
    run_command(cmd_clone)
    os.chdir(qe_dir)
    cmd_fetch_tags = "git fetch --all --tags"
    run_command(cmd_fetch_tags)

    cmd_checkout_files = ["git sparse-checkout add --skip-checks"]
    cmd_checkout_files = " ".join(cmd_checkout_files + helpdoc_files + def_files)
    run_command(cmd_checkout_files)

    # Commands for picking the right versions
    devtools_dir = os.path.join(qe_dir, "dev-tools")
    if not isinstance(versions, list):
        versions = [versions]
    v = versions[0]
    for v in versions:
        print('*************** Working on version v = ', v)
        tag = v
        tag += "MaX" if v in ("6.3", "6.5") else ""
        tag += "MaX-Release" if v == "6.7" else ""
        cmd_checkout_tag = f"git checkout tags/qe-{tag} -b qe-{tag} --force"
        run_command(cmd_checkout_tag)
        database_dir = os.path.join(base_db_dir, "qe-" + v)
        if not os.path.exists(database_dir):
            os.makedirs(database_dir)

        files = [os.path.join(qe_dir, def_file) for def_file in def_files]
        print('*************** Working on def_files = ', files)
        for def_file in files:
            dir = os.path.dirname(def_file)
            # TODO: use python internals?
            cmd_link_xsl = f"ln -sf {devtools_dir}/input_xx.xsl {dir}/input_xx.xsl"
            run_command(cmd_link_xsl)
            cmd_helpdoc = f"{devtools_dir}/helpdoc --version {v} {def_file}"
            run_command(cmd_helpdoc, print_stdout=False)

            # Copy the generated files to the database directory using os module
            xml_file = os.path.splitext(def_file)[0] + ".xml"
            html_file = os.path.splitext(def_file)[0] + ".html"
            # Explicit destination is needed to overwrite existing files
            shutil.move(html_file, os.path.join(database_dir, os.path.basename(html_file)))
            shutil.move(xml_file, os.path.join(database_dir, os.path.basename(xml_file)))
    os.chdir(root)
    # TODO: this is temporary
    # if os.path.exists(work_dir):
    #    shutil.rmtree(work_dir)
