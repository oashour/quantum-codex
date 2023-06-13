"""
Module for dealing with the VASP database (via the wiki)
"""

import logging
import re
import html

import requests

import mwparserfromhell as mwp

API_URL = "https://www.vasp.at/wiki/api.php"
# TODO: include version automatically
USER_AGENT = "dft-codex/0.0.0 (Developer: Omar A. Ashour, ashour@berkeley.edu)"
# API_URL = "https://en.wikipedia.org/w/api.php"


# These are the extra pages we want to parse
EXTRA_PAGE_TITLES = [
    "available PAW potentials",
    "POTCAR",
    "KPOINTS",
    "INCAR",
    "POSCAR",
]


# TODO: move to utils
def standardize_type(t):
    t = t.lower()
    t = re.sub(r"\[(\w+(\s+\(*\w*\)*)*)\]", r" \1", t)
    t = t.replace("(array)", "array").lstrip()
    type_map = {
        "character": "string",
        "float": "real",
        "double": "real",
        "int": "integer",
        "logical": "boolean",
        "bool": "boolean",
    }

    for old_type, new_type in type_map.items():
        if re.search(r"\b" + re.escape(old_type) + r"\b", t):
            return t.replace(old_type, new_type)

    return tidy_wikicode(t)


# TODO: doesn't detect ranges properly (e.g., ISMEAR)
def tidy_options(options, tag_name):
    options = [o.strip() for o in options.split("{{!}}")]
    # Check if boolean
    if len(options) == 1:
        # 1 option: this is a "free" value (e.g., integer, real, etc.)
        return standardize_type(options[0]), {}
    if len(options) == 2:
        # 2 options: might be boolean, check
        options = set([o.lower().strip(".")[0] for o in options])
        if options == set(["t", "f"]):
            clean_options = {
                "True": "...[parsing not implemented]",
                "False": "...[parsing not implemented]",
            }
            return "boolean", clean_options

    # Check for integer ranges
    clean_options = []
    for o in options:
        if re.match(r"\d+-\d+", str(o)):
            start, end = o.split("-")
            clean_options.extend(list(range(int(start), int(end) + 1)))
        else:
            clean_options.append(o)

    # Check if everything can now be converted to an integer
    try:
        clean_options = [int(o) for o in clean_options]
        clean_options = {str(o): "...[parsing not implemented]" for o in clean_options}
        return "integer", clean_options
    except:
        clean_options = {o: "...[parsing not implemented]" for o in clean_options}
        return "string", clean_options


def tidy_wikicode(wikicode, templates=True, formatting=True, strip=True, math=True, unescape=True, footer=True):
    wikicode = str(wikicode)

    if templates:
        pattern_tag = r"\{\{TAG\|\s*(\w+)\s*\}\}"
        pattern_file = r"\{\{FILE\|\s*(\w+)\s*\}\}"
        pattern_tagdef = r"\{\{TAGDEF\|\s*(\w+)\s*\}\}"
        pattern_sc = r"\{\{sc\|(.*?)\}\}"

        wikicode = re.sub(pattern_tag, r"<tag-ref>\1</tag-ref>", wikicode)
        wikicode = re.sub(pattern_file, r"<file-ref>\1</file-ref>", wikicode)
        wikicode = re.sub(pattern_tagdef, r"<tag-ref>\1</tag-ref>", wikicode)
        wikicode = re.sub(pattern_sc, "", wikicode)
        wikicode = re.sub(r"\s*\{\{=\}\}\s*", "=", wikicode)

    if formatting:
        pattern_bold = r"''(.*?)''"
        pattern_italics = r"'''(.*?)'''"

        wikicode = re.sub(pattern_italics, r"<b>\1</b>", wikicode)
        wikicode = re.sub(pattern_bold, r"<i>\1</i>", wikicode)

    if math:
        # TODO: next two can be combined
        wikicode = re.sub(r"\<math\>\s*10\^\{([+-]*\d+)\}\s*\<\/math\>", r"1E\1", wikicode)
        wikicode = re.sub(
            r"\<math\>\s*(\d+)\s*\\times\s*10\^\{([+-]*\d+)\}\s*\<\/math\>", r"\1E\2", wikicode
        )
        # TODO: can be comined
        wikicode = re.sub(r"\<math\>\s*(\d+.\d+)\s*<\/math\>", r"\1", wikicode)
        wikicode = re.sub(r"\<math\>\s*(\d+)\s*<\/math\>", r"\1", wikicode)

    if unescape:
        wikicode = html.unescape(wikicode)

    if strip:
        wikicode = wikicode.strip()

    if footer:
        # Works for the wikicode itself
        wikicode = wikicode.rsplit('----', 1)[0]
        # Works for the mixed wikicode/HTML
        wikicode = wikicode.rsplit('<hr />', 1)[0]

    return wikicode


def get_types_options_defaults(database):
    """
    Get the data types, options, and defaults for each tag in the database
    Works by parsing the wikicode for the TAGDEF and DEF templates
    And manipulating the values contained therein
    """
    tagdef_templates = {}
    def_templates = {}
    for name, tag in database["INCAR"].items():
        wikicode = tag["info"]
        templates = wikicode.filter_templates(
            matches=lambda template: template.name.matches("TAGDEF")
        )
        tagdef_templates[name] = templates[0] if templates else None
        templates = wikicode.filter_templates(matches=lambda template: template.name.matches("DEF"))
        def_templates[name] = templates[0] if templates else None

    for tag_name, tagdef in tagdef_templates.items():
        datatype = "Unknown"
        default = None
        options = {}
        # Figure out data type and possible options
        if tagdef and len(tagdef.params) >= 2:
            datatype, options = tidy_options(tagdef.params[1], tag_name)

        # Figure out defaults
        if tagdef and len(tagdef.params) == 3:
            default = tidy_wikicode(tagdef.params[2])
        elif def_templates[tag_name]:
            dt = def_templates[tag_name].params[1:]
            dt = [tidy_wikicode(d) for d in dt if d]
            if len(dt) == 1:
                default = dt[0]
            else:
                default = " or ".join(f"{dt[i]} ({dt[i+1]})" for i in range(0, len(dt), 2))
        database["INCAR"][tag_name]["type"] = datatype
        database["INCAR"][tag_name]["default"] = default
        database["INCAR"][tag_name]["options"] = options

    return database

def get_descriptions(database):
    """
    Get the descriptions for each tag in the database from its wikicode
    """
    for name, tag in database["INCAR"].items():
        wikicode = tag["info"]
        header = wikicode.split('----', -1)[0]
        match = re.search(r'[Dd]escription\s*:\s*(.*)\s*', header)
        if match:
            database["INCAR"][name]["description"] = tidy_wikicode(match.group(1))
        else:
            database["INCAR"][name]["description"] = "No description available."

    return database


def get_category(category, gcmcontinue=None):
    """
    Gets the titles and last revised date of all pages in a category.
    """
    params = {
        "action": "query",
        "generator": "categorymembers",
        "prop": "revisions",
        "gcmlimit": "max",
        "gcmtitle": category,
        "format": "json",
        "formatversion": "2",
    }
    if gcmcontinue:
        params["gcmcontinue"] = gcmcontinue

    headers = {"User-Agent": USER_AGENT}
    req = requests.get(API_URL, headers=headers, params=params).json()

    # Figure out if we need to parse more pages
    gcmcontinue = None
    if "continue" in req:
        gcmcontinue = req["continue"]["gcmcontinue"]

    pages = []
    for page in req["query"]["pages"]:
        pages.append(
            {
                "title": page["title"],
                "type": None,
                "options": {},
                "default": "",
                "info": None,
                "html": None,
                "id": page["pageid"],
                "last_revised": page["revisions"][0]["timestamp"],
            }
        )

    return pages, gcmcontinue


def get_incar_tags(get_text=True):
    """
    Gets the titles and possibly text of all pages in the "Category:INCAR tag" category.
    """
    gmcontinue = None
    pages, gcmcontinue = get_category("Category:INCAR tag")
    while gcmcontinue is not None:
        cont_pages, gcmcontinue = get_category("Category:INCAR tag", gcmcontinue=gmcontinue)
        pages.extend(cont_pages)

    # This entry breaks the wiki...
    bad_entry = "Construction:LKPOINTS WAN"
    titles = [page["title"] for page in pages]
    if bad_entry in titles:
        # Pop from pages
        pages.pop(titles.index(bad_entry))

    if get_text:
        page_titles = [page["title"] for page in pages]
        pages = get_from_vasp_wiki(page_titles)
    return pages


def get_from_vasp_wiki(title, get_text=True):
    """
    Parse a list of page titles and return a list of dicts with the pageid,
    title, timestamp, and wiki text (if requested).
    """
    pages = []

    rvprop = "timestamp"
    if get_text:
        rvprop += "|content"
    if isinstance(title, list):
        if len(title) > 50:
            chunks = [title[x : x + 50] for x in range(0, len(title), 50)]
            for chunk in chunks:
                pages.extend(get_from_vasp_wiki(chunk, get_text=get_text))
            return pages
        else:
            title = "|".join(title)
    params = {
        "action": "query",
        "prop": "revisions",
        "rvprop": rvprop,
        "titles": title,
        "format": "json",
        "formatversion": "2",
    }
    if get_text:
        params["rvslots"] = "main"
    headers = {"User-Agent": USER_AGENT}

    req = requests.get(API_URL, headers=headers, params=params).json()
    try:
        req_pages = req["query"]["pages"]
    except:
        raise ValueError(f"Something went wrong..., this is the response I got.\n. {req}")

    for page in req_pages:
        pageid = page["pageid"]
        title = page["title"]
        rev = page["revisions"][0]
        timestamp = rev["timestamp"]
        text = None
        if get_text:
            text = mwp.parse(rev["slots"]["main"]["content"])
        pages.append(
            {
                "title": title,
                "type": None,
                "dimension": 1,
                "options": {},
                "default": "",
                "info": text,
                "html": None,
                "id": pageid,
                "last_revised": timestamp,
            }
        )
    return pages


# EDGE CASES
# TODO: there's a tag called 'NMAXFOCKAE and NMAXFOCKAE'
# TODO: NHC_NS has problem with unncessary data type and options in default
# TODO: KPOINTS_OPT_NKBATCH has some weirdness
# TODO: some options look like ['hi', 'bye (or goodbye)', 'hello']
# TODO: some links [[GW approximation of Hedin's equations#lowGW|GW]]
# TODO: [[GW_calculations|GW calculations]] and [[ACFDT_calculations|ACFDT calculations]]
# TODO [[GW calculations]] [[ACFDT calculations]]
def generate_database():
    """
    Generates the database from the wiki.
    """
    database = {}
    pages = get_from_vasp_wiki(EXTRA_PAGE_TITLES, get_text=True)
    database["extras"] = {page["title"]: page for page in pages}
    pages = get_incar_tags(get_text=True)
    database["INCAR"] = {page["title"].replace(" ", "_"): page for page in pages}
    database = get_types_options_defaults(database)
    database = get_descriptions(database)

    return database


# TODO: update to work with get_types_options_defaults
def refresh_database(database):
    """
    Refreshes the database with the latest information from the wiki.
    """
    # Get only last revised dates
    last_revisions = {}
    pages = get_from_vasp_wiki(EXTRA_PAGE_TITLES, get_text=False)
    last_revisions["extras"] = {page["title"]: page for page in pages}
    pages = get_incar_tags(get_text=False)
    last_revisions["INCAR"] = {page["title"]: page for page in pages}

    # See if anything is missing or outdated
    pages_to_update = {}
    for category, pages in last_revisions.items():
        pages_to_update[category] = []
        for title, page in pages.items():
            if (
                title not in database[category]
                or page["last_revised"] != database[category][title]["last_revised"]
            ):
                pages_to_update[category].append(title)

    # Pull the new data
    for category, pages in pages_to_update.items():
        if pages:
            logging.info("Updating the following pages:" + ", ".join(pages))
            updated_pages = get_from_vasp_wiki(pages_to_update[category], get_text=True)
            for page in updated_pages:
                title = page["title"]
                database[category][title] = page
    else:
        logging.info("Database is up to date.")

    return database
