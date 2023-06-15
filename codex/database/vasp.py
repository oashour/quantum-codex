"""
Module for dealing with the VASP database (via the wiki)
"""

import logging
import re
import html
import json
from importlib import resources
import os
from urllib.parse import quote
from datetime import timezone, datetime


import requests
import mwparserfromhell as mwp
import mwcomposerfromhell as mwc
from lxml.html import parse, fromstring, tostring

API_URL = "https://www.vasp.at/wiki/api.php"
# TODO: include version automatically
USER_AGENT = "dft-codex/0.0.0 (Developer: Omar A. Ashour, ashour@berkeley.edu)"
WIKI_URL = "https://www.vasp.at/wiki/index.php"


# These are the extra pages we want to parse
EXTRA_PAGE_TITLES = [
    "available PAW potentials",
    "POTCAR",
    "KPOINTS",
    "INCAR",
    "POSCAR",
]


def standardize_type(t):
    """
    Standardizes data types to something more uniform
    """
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
def tidy_options(options):
    """
    Cleans up the options of a VASP tag and figures out data type.
    """
    options = [o.strip() for o in options.split("{{!}}")]
    # Check if boolean
    if len(options) == 1:
        # 1 option: this is a "free" value (e.g., integer, real, etc.)
        return standardize_type(options[0]), {}
    if len(options) == 2:
        # 2 options: might be boolean, check
        options = set(o.lower().strip(".")[0] for o in options)
        if options == set(["t", "f"]):
            clean_options = {
                ".TRUE.": None,
                ".FALSE.": None,
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
        clean_options = {str(o): None for o in clean_options}
        return "integer", clean_options
    except:
        clean_options = {o: None for o in clean_options}
        return "string", clean_options


# TODO: decide on something uniform regarding HTML tags
def tidy_wikicode(
    wikicode,
    templates=True,
    formatting=True,
    strip=True,
    math=True,
    unescape=True,
    links=True,
    style_tag_values=True,
):
    """
    Tidies the wikicode by removing templates, formatting, etc.
    """
    wikicode = str(wikicode)
    # Makes sure we don't have any empty/whitespace-only strings
    if not wikicode.strip():
        return wikicode

    # TODO: turn everything into a global re.compile to speed up
    if templates:
        tag_patterns = [
            (r"\{\{TAG\|\s*(\w+)\s*\}\}", r'<a class="tag-link" href={url}>\1</a>'),
            (r"\{\{FILE\|\s*(\w+)\s*\}\}", r'<a class="tag-link" href={url}>\1</a>'),
            (r"\{\{TAGDEF\|\s*(\w+)\s*\}\}", r'<a class="tag-link" href={url}>\1</a>'),
        ]
        for pattern, replacement in tag_patterns:
            match = re.search(pattern, wikicode)
            if match:
                url = WIKI_URL + "/" + quote(match.group(1).replace(" ", "_"))
                wikicode = re.sub(pattern, replacement.format(url=url), wikicode)

        wikicode = re.sub(r"\{\{sc\|(.*?)\}\}", "", wikicode)
        wikicode = re.sub(r"\s*\{\{=\}\}\s*", "=", wikicode)
        wikicode = re.sub(r"\{\{CITE\|(.*?)\}\}", "", wikicode, flags=re.IGNORECASE)

    if links:
        link_patterns = [
            (r"\[\[([^|]*?)\]\]", r'<a href="{url}">\1</a>'),
            (r"\[\[(.*?)\|(.*?)\]\]", r'<a href="{url}">\2</a>'),
        ]

        for pattern, replacement in link_patterns:
            match = re.search(pattern, wikicode)
            if match:
                url = WIKI_URL + "/" + quote(match.group(1).replace(" ", "_"))
                wikicode = re.sub(pattern, replacement.format(url=url), wikicode)
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
    if style_tag_values:
        el = fromstring(wikicode)
        if el.xpath("//a[@class='tag-link']"):
            # Iterate over links with class "tag-link" and style values
            for a in el.xpath("//a[@class='tag-link']"):
                _style_tag_values(a)
            wikicode = tostring(el, encoding="unicode")
            # These get added when converting to HTML
            for tag in ["<p>", "<span>", "</p>", "</span>"]:
                wikicode = wikicode.replace(tag, "")
    if unescape:
        wikicode = html.unescape(wikicode)

    if strip:
        wikicode = wikicode.strip()

    return wikicode


def tidy_page_html(page_html, page_name):
    """
    Tidies the HTML of a VASP wiki page
    """
    root = fromstring(page_html)

    for a in root.xpath("//a"):
        # These are self links, so we want to make them monospace
        if a.attrib.get("class") == "mw-selflink selflink":
            a.classes.remove("mw-selflink")
            a.classes.remove("selflink")
            a.classes.add("tag-link")
            # Using a.text instead of page name causes problems with links that
            # enclose other HTML elements (e.g., <b> or <span>, happens on some pages...)
            a.attrib["href"] = WIKI_URL + page_name
            # Now we check if the tail has '= something' in it
            _style_tag_values(a)

        elif a.attrib.get("href"):
            page = a.attrib["href"].split("/")[-1]
            # This is our best guess for pages that link to files or other tags
            if page.upper() == page:
                a.classes.add("tag-link")
                _style_tag_values(a)
    for table in root.xpath("//table"):
        # This is a hack to find the warning/mind/important etc tables
        if table.attrib.get("style"):
            # Not very robust if VASP change their template but not many options...
            # style = table.attrib["style"].split(";")
            # style = [s for s in style if s.strip()]
            # style = dict(s.split(":") for s in style)
            # style = {k.strip(): v.strip() for k, v in style.items()}
            # if style.get("border") and style.get("padding"):
            #    if style["border"].startswith("0px") and style["padding"] == "5px":
            alert_class = None
            for s in table.xpath(".//b//span"):
                heading = s.text
                heading_element = s.getparent()
                tail = heading_element.tail
                td = heading_element.getparent()
                if s.text.lower().startswith("important"):
                    alert_class = "alert-primary"
                elif s.text.lower().startswith("warning"):
                    alert_class = "alert-warning"
                elif s.text.lower().startswith("mind"):
                    alert_class = "alert-info"
                elif s.text.lower().startswith("tip"):
                    alert_class = "alert-success"
                elif s.text.lower().startswith("deprecated"):
                    alert_class = "alert-danger"
                else:
                    alert_class = "alert-secondary"
                children = td.getchildren()
                children.remove(heading_element)
                heading_element
                break
            new_element = fromstring(
                f'<div class="alert {alert_class}" role="alert">'
                f'<h4 class="alert-heading">{heading}</h4>{tail}</div>'
            )
            new_element.extend(children)
            table.getparent().replace(table, new_element)
        else:
            table.classes.add("table")
            table.classes.add("table-striped")
            table.classes.add("table-hover")
            if "wikitable" in table.attrib.get("class"):
                table.classes.remove("wikitable")

    return tostring(root).decode("utf-8")


def tidy_tag_html(tag_html, tag_name):
    """
    Tidies the HTML of an INCAR tag page.
    """
    # TODO: these are not robust if something changes in the VASP wiki (e.g., <hr/> not <hr />)
    # Get rid of footer
    tag_html = tag_html.rsplit("<hr />", 1)[0]
    # Get rid of header
    tag_html = tag_html.rsplit("<hr />", 1)[-1]

    return tidy_page_html(tag_html, tag_name)


def _style_tag_values(a):
    """This styles tag values
    Takes an anchor element, which could be something like (in HTML):
    <a href="https://www.vasp.at/wiki/index.php/ALGO" title="ALGO">ALGO</a>=Normal bla bla bla
    And changes it to
    <a href="https://www.vasp.at/wiki/index.php/ALGO" title="ALGO">ALGO</a>=<span class="tag-value">Normal</span> bla bla bla
    by adding a new span element after it in the tree and changing its tail
    """
    # TODO: can be split into two functions
    if a.tail and a.tail.startswith("="):
        match = re.match(r"\s*=\s*([^\s]+)(.*)", a.tail, re.S)
        if match:
            a.tail = "="
            index = a.getparent().index(a)
            # TODO: get rid of style (temporary for testing)
            new_element = fromstring(
                f'<span class="tag-value" style="color: red;">{match.group(1)}</span>'
            )
            new_element.tail = match.group(2)
            a.getparent().insert(index + 1, new_element)
    elif (
        a.getnext() is not None
        and a.getnext().tag == "math"
        and (a.getnext().text.strip(" ") in ("=", "\neq", "\leq", "\geq"))
    ):
        math_el = a.getnext()
        match = re.match(r"\s*([^\s]+)(.*)", math_el.tail, re.S)
        if match:
            math_el.tail = ""
            index = math_el.getparent().index(math_el)
            # TODO: get rid of style (temporary for testing)
            new_element = fromstring(
                f'<span class="tag-value" style="color: red;">{match.group(1)}</span>'
            )
            new_element.tail = match.group(2)
            a.getparent().insert(index + 1, new_element)


# EDGE CASES
# TODO: there's a tag called 'NMAXFOCKAE and NMAXFOCKAE'
# TODO: NHC_NS has problem with unncessary data type and options in default
# TODO: KPOINTS_OPT_NKBATCH has some weirdness
# TODO: some options look like ['hi', 'bye (or goodbye)', 'hello']
def parse_incar_tag(page, html_dict):
    """
    Get the data types, options, and defaults for each tag in the database
    Works by parsing the wikicode for the TAGDEF and DEF templates
    And manipulating the values contained therein

    See get_incar_tags docstring for an explanation of html dict
    """
    wikicode = page["text"]

    templates = wikicode.filter_templates(matches=lambda template: template.name.matches("TAGDEF"))
    tagdef_temp = templates[0] if templates else None
    templates = wikicode.filter_templates(matches=lambda template: template.name.matches("DEF"))
    def_temp = templates[0] if templates else None

    datatype = None
    default = None
    summary = None
    options = {}

    if tagdef_temp and len(tagdef_temp.params) >= 2:
        datatype, options = tidy_options(tagdef_temp.params[1])

    # Figure out default options (either 4th argument of TAGDEF or 2nd+ arguments of DEF)
    if tagdef_temp and len(tagdef_temp.params) == 3:
        default = tidy_wikicode(tagdef_temp.params[2])
    elif def_temp:
        dt = def_temp.params[1:]
        dt = [tidy_wikicode(d) for d in dt if d.strip()]
        if len(dt) == 1:
            default = dt[0]
        else:
            default = [(dt[i], dt[i + 1]) for i in range(0, len(dt), 2)]

    # Figure out summary
    header = wikicode.split("----", -1)[0]
    match = re.search(r"\s*[Dd]escription\s*:\s*(.*)\s*", header)
    if match:
        summary = tidy_wikicode(match.group(1))

    tag_html = None
    name = page["title"].replace(" ", "_")
    # If passed a cached HTML dictionary, use that
    if html_dict:
        tag_html = html_dict.get(name, None)
    # If no cache or it doesn't have the tag, try to query the wiki
    if tag_html is None:
        tag_html = get_raw_html(page["title"])

    tag = {
        "datatype": datatype,
        "default": default,
        "options": options,
        "summary": summary,
        "id": page["pageid"],
        "info": tidy_tag_html(tag_html, name),
        "last_revised": page["last_revised"],
    }

    return name, tag


def parse_wiki_page(page):
    """
    Parses a wiki page and returns a dictionary with the relevant information.
    """
    title = page["title"]
    # TODO: generate summary
    summary = None
    page_html = get_raw_html(page["title"])
    page = {
        "datatype": None,
        "default": None,
        "options": None,
        "summary": summary,
        "id": page["pageid"],
        "info": tidy_page_html(page_html, page["title"]),
        "last_revised": page["last_revised"],
    }

    return title, page


def get_category(category):
    """
    Gets the titles, pageid and last revised date of all pages in a category.
    Takes care of continuation if not everything was returned.
    """
    pages, gcmcontinue = _get_partial_category(category, None)
    while gcmcontinue is not None:
        cont_pages, gcmcontinue = get_category(category, gcmcontinue)
        pages.extend(cont_pages)

    return _kill_bad_entries(pages)


def _get_partial_category(category, gcmcontinue):
    """
    Gets as many pages from a category as possible.
    Use get_category instead.
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
                "pageid": page["pageid"],
                "last_revised": page["revisions"][0]["timestamp"],
            }
        )

    return pages, gcmcontinue


def _kill_bad_entries(pages):
    """
    Remove bad entries from the list of pages
    """
    # This entry breaks the wiki...
    bad_entry = "Construction:LKPOINTS WAN"
    titles = [page["title"] for page in pages]
    if bad_entry in titles:
        # Pop from pages
        pages.pop(titles.index(bad_entry))

    return pages


def get_incar_tags(html_json_path=None):
    """
    Gets the titles and possibly text of all pages in the "Category:INCAR tag" category.
    html_json_path is the path to a json file containing the raw HTML of the pages.
    Format is {"tag_name": "html"},
    where tag_name is the actual name of the tag with underscores instead of spaces.

    It takes about 8-10 minutes to generate the raw HTML for all the INCAR tag pages.
    So if you're generating the entire database from scratch, you can provide
    the json file to speed things up. It would be quicker to do that then refresh the
    database since the wiki isn't updated very regularly.
    """
    pages = get_category("Category:INCAR tag")
    if html_json_path:
        with open(html_json_path, "r") as f:
            html_dict = json.load(f)
    else:
        html_dict = {}

    tags = {}
    page_titles = [page["title"] for page in pages]
    pages = get_from_vasp_wiki(page_titles)
    for p in pages:
        name, tag = parse_incar_tag(p, html_dict)
        tags[name] = tag

    return tags


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
        # You can't send a request for more than 50 pages. Break into chunks.
        if len(title) > 50:
            chunks = [title[x : x + 50] for x in range(0, len(title), 50)]
            for chunk in chunks:
                pages.extend(get_from_vasp_wiki(chunk, get_text=get_text))
            return pages
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
                "text": text,
                "pageid": pageid,
                "last_revised": timestamp,
            }
        )
    return pages


def get_raw_html(title):
    """
    Gets the HTML of a wiki page using the render action.
    This is almost unstyled HTML that is then styled by
    tidy_page_html or tidy_tag_html.

    Returns:
        str: The HTML of the wiki page.
    """
    params = {
        "title": title,
        "action": "render",
    }
    headers = {"User-Agent": USER_AGENT}

    req = requests.get(WIKI_URL, headers=headers, params=params)
    try:
        req.raise_for_status()
        return req.text
    except requests.exceptions.HTTPError as e:
        raise e


def generate_database(use_cached_html=True):
    """
    Generates the database from the wiki.
    """
    database = {}
    # INCAR tags
    base_db_dir = resources.files("codex.database")
    html_json_path = None
    if use_cached_html:
        html_json_path = os.path.join(base_db_dir, "vasp-1686736265", "incar-raw-html.json")
    database["INCAR"] = get_incar_tags(html_json_path=html_json_path)
    # TODO: files category
    # Extras
    pages = get_from_vasp_wiki(EXTRA_PAGE_TITLES, get_text=True)
    database["extras"] = {}
    for page in pages:
        title, page = parse_wiki_page(page)
        database["extras"][title] = page

    timestamp = str(datetime.now(timezone.utc).timestamp()).split(".", maxsplit=1)[0]
    database_dir = "vasp-" + timestamp
    database_dir = "vasp-1686736265" # TODO: this is temporary
    db_filename = os.path.join(base_db_dir, database_dir, f"database.json")
    with open(db_filename, "w") as f:
        json.dump(database, f)

    return database


def refresh_database(database):
    """
    Refreshes the database with the latest information from the wiki.
    """
    # Get only last revised dates
    last_revisions = {}
    pages = get_from_vasp_wiki(EXTRA_PAGE_TITLES, get_text=False)
    last_revisions["extras"] = {page["title"]: page for page in pages}
    pages = get_category("Category:INCAR tag")
    last_revisions["INCAR"] = {page["title"]: page for page in pages}

    # See if anything is missing or outdated
    pages_to_update = {}
    for category, pages in last_revisions.items():
        pages_to_update[category] = []
        for title, page in pages.items():
            if category == "INCAR":
                title = title.replace(" ", "_")
            if (
                title not in database[category]
                or page["last_revised"] != database[category][title]["last_revised"]
            ):
                pages_to_update[category].append(title)

    # Check if there's anything to update
    if all(not lst for lst in pages_to_update.values()):
        logging.info("The database is up to date.")
        return database

    # Pull the new data
    for category, pages in pages_to_update.items():
        if pages:
            logging.info(f"Found {len(pages)} entries requiring updates in category {category}.")
            logging.info("Updating the following entries: " + ", ".join(pages))
            updated_pages = get_from_vasp_wiki(pages, get_text=True)

            for page in updated_pages:
                if category == "INCAR":
                    title, page = parse_incar_tag(page, None)
                else:
                    title, page = parse_wiki_page(page)
            database[category][title] = page

    return database
