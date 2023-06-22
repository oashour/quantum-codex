"""
Module for dealing with the VASP JSON (via the wiki)
"""

import logging
import re
import html
import json
from importlib import resources
from importlib.metadata import metadata
import os
from urllib.parse import quote
from datetime import timezone, datetime


import requests
import mwparserfromhell as mwp
from lxml.html import fromstring, tostring

from codex.database.utils import standardize_type

API_URL = "https://www.vasp.at/wiki/api.php"
WIKI_URL = "https://www.vasp.at/wiki/index.php"
metadata = metadata("dft-codex")
__version__ = metadata["Version"]
__maintainer__ = metadata["Maintainer"]
__maintainer_email__ = metadata["Maintainer-email"]
USER_AGENT = f"dft-codex/{__version__} (Developer: {__maintainer__}, {__maintainer_email__})"


# Extra pages to pull from VASP wiki
EXTRA_PAGE_TITLES = [
    "available PAW potentials",
    "POTCAR",
    "KPOINTS",
    "INCAR",
    "POSCAR",
]


def generate_json(use_cached_json=True, use_cached_html=True, use_cached_options=True):
    """
    Generates a JSON file from the wiki to be fed into the database.
    params:
        use_cached_json (bool): Whether to use the cached JSON file. The function will look for the
                                latest JSON file in the json/ directory and use that if it exists.
                                Much faster, highly recommended.
        use_cached_html (bool): Whether to use the cached raw HTML of the wiki pages.
                                The VASP wiki is updated irregularly so the cache works really well.
                                If the page is not in the cache or it's been updated since, it will
                                be fetched from the wiki. Set to False to forces a full referesh
                                (takes about 10 minutes, and adds unnecessary pressure to VASP
                                wiki servers. highly not recommended).
        use_cached_options (bool): Whether to use the human-summarized descriptions
                                   of the options of each tag. If False, the description
                                   of each option will just be None
    """
    json_cache, options_cache, html_cache = _get_caches(
        use_cached_json, use_cached_html, use_cached_options
    )

    if json_cache:
        vasp_wiki_dict = _refresh_json_from_cache(json_cache, html_cache, options_cache)
    else:
        vasp_wiki_dict = _build_json_from_scratch(html_cache, options_cache)

    # Sava JSON to json/vasp-{timestamp}.json
    base_db_dir = resources.files("codex.database")
    timestamp = str(datetime.now(timezone.utc).timestamp()).split(".", maxsplit=1)[0]
    json_filename = os.path.join(base_db_dir, "json", "vasp-" + timestamp + ".json")
    with open(json_filename, "w") as f:
        json.dump(vasp_wiki_dict, f, indent=4)

    return vasp_wiki_dict


# TODO: make this check for new HTML and see if options are outdated
def _build_json_from_scratch(html_cache, options_cache):
    """
    Builds the VASP wiki JSON from scratch
    """
    # INCAR tags
    vasp_wiki_dict = {}
    vasp_wiki_dict["INCAR"] = _get_incar_tags(html_cache, options_cache)

    # TODO: files category?

    # Extras
    pages = _get_from_vasp_wiki(EXTRA_PAGE_TITLES, get_text=True)
    vasp_wiki_dict["extras"] = []
    for page in pages:
        vasp_wiki_dict["extras"].append(_parse_wiki_page(page))

    return vasp_wiki_dict


# TODO: update to work with new JSON format (list of dicts)
def _refresh_json_from_cache(json_cache, html_cache, options_cache):
    """
    Regenerates a vasp wiki dict from a cached JSON file, only pulling new data from the wiki
    as necessary.
    """
    # Get only last revised dates
    last_revisions = {}
    pages = _get_from_vasp_wiki(EXTRA_PAGE_TITLES, get_text=False)
    last_revisions["extras"] = [(p["title"], p["last_revised"]) for p in pages]
    pages = _get_category("Category:INCAR tag")
    last_revisions["INCAR"] = [(p["title"].replace(" ", "_"), p["last_revised"]) for p in pages]

    # See if anything is missing or outdated
    pages_to_update = {}
    for category, new_pages in last_revisions.items():
        pages_to_update[category] = []
        cached_pages = [(p["name"], p["last_revised"]) for p in json_cache[category]]
        for title, last_revised in new_pages:
            if (title, last_revised) not in cached_pages:
                index = next(
                    (i for i, d in enumerate(json_cache[category]) if d["name"] == title), None
                )
                pages_to_update[category].append((title, index))

    # Check if there's anything to update
    if all(not l for l in pages_to_update.values()):
        logging.info("The JSON file is already up to date.")
        return json_cache

    # Pull the new data
    for category, pages in pages_to_update.items():
        if pages:
            logging.info(f"Found {len(pages)} entries requiring updates in category {category}.")
            page_titles = [p[0] for p in pages]
            logging.info("Updating the following entries: " + ", ".join(page_titles))
            page_indices = [p[1] for p in pages]
            updated_pages = _get_from_vasp_wiki(page_titles, get_text=True)

            for p, i in zip(updated_pages, page_indices):
                if category == "INCAR":
                    page = _parse_incar_tag(p, html_cache, options_cache)
                else:
                    page = _parse_wiki_page(p)
                if i is None:
                    logging.info(f"Appending {p['title']} to the JSON file.")
                    json_cache[category].append(page)
                else:
                    logging.info(f"Updating {p['title']} in the JSON file.")
                    logging.info(f"Old entry: {json_cache[category][i]}")
                    json_cache[category][i] = page

    return json_cache


# TODO: doesn't detect ranges properly (e.g., ISMEAR has [integer] > 0)
def _tidy_options(options):
    """
    Cleans up the options of a VASP tag and figures out data type.
    """
    options = [o.strip() for o in options.split("{{!}}")]
    # Check if boolean
    if len(options) == 1:
        # 1 option: this is a "free" value (e.g., integer, real, etc.)
        datatype = _tidy_wikicode(standardize_type(options[0]))
        return datatype, {}
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


def _tidy_wikicode(wikicode):
    """
    Tidies the wikicode by removing/converting templates, formatting, etc.

    This is not meant to be a wikicode parser, just something that deals with a few
    specific cases from the VASP wiki
    """
    wikicode = str(wikicode)
    # Makes sure we don't have any empty/whitespace-only strings
    if not wikicode.strip():
        return wikicode

    # TODO: turn everything into a global re.compile to speed up
    # TODO: need a pattern for self tag template to add class selflink to anchor
    # Templates
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

    # Links
    link_patterns = [
        (r"\[\[([^|]*?)\]\]", r'<a href="{url}">\1</a>'),
        (r"\[\[(.*?)\|(.*?)\]\]", r'<a href="{url}">\2</a>'),
    ]

    for pattern, replacement in link_patterns:
        match = re.search(pattern, wikicode)
        if match:
            url = WIKI_URL + "/" + quote(match.group(1).replace(" ", "_"))
            wikicode = re.sub(pattern, replacement.format(url=url), wikicode)

    # Formatting
    pattern_bold = r"''(.*?)''"
    pattern_italics = r"'''(.*?)'''"
    wikicode = re.sub(pattern_italics, r"<b>\1</b>", wikicode)
    wikicode = re.sub(pattern_bold, r"<i>\1</i>", wikicode)

    # Math
    # TODO: next two can be combined
    wikicode = re.sub(r"\<math\>\s*10\^\{([+-]*\d+)\}\s*\<\/math\>", r"1E\1", wikicode)
    wikicode = re.sub(
        r"\<math\>\s*(\d+)\s*\\times\s*10\^\{([+-]*\d+)\}\s*\<\/math\>", r"\1E\2", wikicode
    )
    # TODO: these two can also be comined
    wikicode = re.sub(r"\<math\>\s*(\d+.\d+)\s*<\/math\>", r"\1", wikicode)
    wikicode = re.sub(r"\<math\>\s*(\d+)\s*<\/math\>", r"\1", wikicode)

    # Style tags
    el = fromstring(wikicode)
    if el.xpath("//a[@class='tag-link']"):
        # Iterate over links with class "tag-link" and style values
        for a in el.xpath("//a[@class='tag-link']"):
            _style_tag_values(a)
        wikicode = tostring(el, encoding="unicode")
        # These get added when converting to HTML
        # This also adds a bunch of useless <span> tags
        for tag in ["<p>", "</p>"]:
            wikicode = wikicode.replace(tag, "")

    # Unescape and strip
    wikicode = html.unescape(wikicode)
    wikicode = wikicode.strip()

    return wikicode


def _remove_tag_header_footer(root):
    """
    Removes the headers and footers from a VASP wiki page (INCAR tags)
    Everything before and up to the <p> element with text "Description:" is removed (header)
    Everything from the <h2> element with text "Related tags and articles" is removed (footer)
    Mutating function
    """
    # Translate is a hacky way to get case insensitivity in xpath
    # You need to really know the HTML layout to understand how this works...
    # This is not optimal, need some documentation for maintainers
    header = root.xpath("//*[starts-with(translate(text(), 'D', 'd'), 'description:')]")
    for element in header:
        preceding_siblings = element.xpath("preceding-sibling::*")
        for sibling in preceding_siblings:
            sibling.getparent().remove(sibling)
        element.getparent().remove(element)
        # Get next <hr> sibling

    footer = root.xpath("//*[starts-with(translate(text(), 'RT', 'rt'), 'related tags')]")
    for element in footer:
        parent = element.getparent()
        following_siblings = parent.xpath("following-sibling::*")
        for sibling in following_siblings:
            sibling.getparent().remove(sibling)
        parent.getparent().remove(parent)
    examples_link = root.xpath("//a[starts-with(translate(text(), 'E', 'e'), 'examples')]")
    for element in examples_link:
        parent = element.getparent()
        parent.getparent().remove(parent)

    # Removes the very horizontal rule in the page
    for el in root.xpath("//hr"):
        el.getparent().remove(el)
        break


def _tidy_page_html(page_html, page_name, remove_header_footer=True):
    """
    Tidies the HTML of a VASP wiki page
    """
    root = fromstring(page_html)
    if remove_header_footer:
        _remove_tag_header_footer(root)

    for a in root.xpath("//a"):
        # These are self links, so we want to make them monospace
        if a.attrib.get("class") == "mw-selflink selflink":
            a.classes.remove("mw-selflink")
            # a.classes.remove("selflink")
            a.classes.add("tag-link")
            # Using a.text instead of page name causes problems with links that
            # enclose other HTML elements (e.g., <b> or <span>, happens on some pages...)
            a.attrib["href"] = WIKI_URL + "/" + page_name
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
                f'<h5 class="alert-heading">{heading.strip(":")}</h5>{tail}</div>'
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


def _style_tag_values(a):
    """
    This styles tag values
    Takes an anchor element, which could be something like (in HTML):
    <a href="..." title="ALGO">ALGO</a>=Normal bla bla bla
    And changes it to
    <a href="..." title="ALGO">ALGO</a>=<span class="tag-value">Normal</span> bla bla bla
    by adding a new span element after it in the tree and changing its tail.
    Can also detect things like
    <a href="..." title="ISMEAR">ISMEAR</a><math>\neq<\math>0 bla bla bla
    """
    tail = html.unescape(a.tail.lstrip(" ")) if a.tail else ""
    # Splitting .TRUE., .FALSE. and floats since they have . in them
    match = re.match(
        r"\s*(=|!=|>|<|>=|<=|≥|≤)\s*([^\s.,:]+|\.TRUE\.|\.FALSE\.|\d+.\d+[^\s]*)(.*)", tail, re.S
    )
    if match:
        a.tail = match.group(1)
        index = a.getparent().index(a)
        new_element = fromstring(
            f'<span class="tag-value">{match.group(2)}</span>' f"{match.group(3)}"
        )
        a.getparent().insert(index + 1, new_element)
    elif (
        a.getnext() is not None
        and a.getnext().tag == "math"
        and (a.getnext().text.strip(" ") in ("=", ">", "<", "\neq", "\leq", "\geq"))
    ):
        math_el = a.getnext()
        match = re.match(r"\s*([^\s.,:]+|\.TRUE\.|\.FALSE\.)(.*)", math_el.tail, re.S)
        if match:
            math_el.tail = ""
            index = math_el.getparent().index(math_el)
            new_element = fromstring(
                f'<span class="tag-value">{match.group(1)}</span>' f"{match.group(2)}"
            )
            a.getparent().insert(index + 1, new_element)


# TODO: EDGE CASES
# 1. there's a tag called 'NMAXFOCKAE and NMAXFOCKAE'
# 2. NHC_NS has problem with unncessary data type and options in default
# 3. KPOINTS_OPT_NKBATCH has some weirdness
def _parse_incar_tag(page, html_cache, options_cache):
    """
    Get the data types, options, and defaults for each INCAR tag
    Works by parsing the wikicode for the TAGDEF and DEF templates
    And manipulating the values contained therein.

    params:
        page: dict in the style of get_from_vasp_wiki
        html_cache: dict of {tag_name: {'html': ..., 'last_revised': ...}}
        options_cache: dict of {tag_name: {'options': ..., 'last_revised': ...}}
    """
    wikicode = page["text"]
    name = page["title"].replace(" ", "_")


    templates = wikicode.filter_templates(matches=lambda template: template.name.matches("TAGDEF"))
    tagdef_temp = templates[0] if templates else None
    templates = wikicode.filter_templates(matches=lambda template: template.name.matches("DEF"))
    def_temp = templates[0] if templates else None

    datatype = None
    default = None
    summary = None
    options = {}

    if tagdef_temp and len(tagdef_temp.params) >= 2:
        datatype, options = _tidy_options(tagdef_temp.params[1])

    # Figure out default options (either 4th argument of TAGDEF or 2nd+ arguments of DEF)
    if tagdef_temp and len(tagdef_temp.params) == 3:
        default = _tidy_wikicode(tagdef_temp.params[2])
    elif def_temp:
        dt = def_temp.params[1:]
        dt = [_tidy_wikicode(d) for d in dt if d.strip()]
        if len(dt) == 1:
            default = dt[0]
        else:
            default = [(dt[i], dt[i + 1]) for i in range(0, len(dt), 2)]

    # Figure out summary
    header = wikicode.split("----", 1)[0]
    # Take care of some typos
    header = header.replace("Descprition", "Description")
    header = header.replace("Desription", "Description")
    match = re.search(r"\s*[Dd]escription\s*\s*(.*)\s*", header)
    if match:
        summary = _tidy_wikicode(match.group(1).strip(":"))

    # Get options from cache if it's there, throw warning if it's out of date
    # These are presumably human-authored, and will override the parsed ones
    tag_options = options_cache.get(name, {})
    if tag_options:
        if tag_options["last_revised"] != page["last_revised"]:
            logging.warning(f"Options for {name} are out of date, will still use them.")
        options = tag_options["options"]

    # Get HTML from Cache if it's there, get it otherwise not up to date
    tag_html = html_cache.get(name, None)
    if tag_html and tag_html["last_revised"] == page["last_revised"]:
        tag_html = tag_html["html"]
    else:
        # TODO: need mechanism to save this back to the cache
        logging.info(f"Getting HTML for {name} from wiki (outdated or not in cache).")
        tag_html = _get_raw_html(page["title"])

    tag = {
        "name": name,
        "section": None,
        "datatype": datatype,
        "default": default,
        "options": options,
        "summary": summary,
        "id": page["pageid"],
        "info": _tidy_page_html(tag_html, name),
        "last_revised": page["last_revised"],
    }

    return tag


def _parse_wiki_page(page):
    """
    Parses a generic wiki page and returns a dictionary with the relevant information.
    Intended for use with non-INCAR tag pages.
    """
    page_html = _get_raw_html(page["title"])
    page = {
        "name": page["title"],
        "info": _tidy_page_html(page_html, page["title"], remove_header_footer=False),
        "id": page["pageid"],
        "last_revised": page["last_revised"],
    }

    return page


def _get_category(category):
    """
    Gets the titles, pageid and last revised date of all pages in a category.
    Takes care of continuation if not everything was returned.
    """

    def get_partial_category(category, gcmcontinue):
        # MediaWiki API returns only a certain subset of a category. To get all pages, we need to
        # use the "gcmcontinue" parameter in the GET request
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

    pages, gcmcontinue = get_partial_category(category, None)
    while gcmcontinue is not None:
        cont_pages, gcmcontinue = _get_category(category, gcmcontinue)
        pages.extend(cont_pages)

    return _kill_bad_entries(pages)


def _kill_bad_entries(pages):
    """
    Remove bad entries from the list of pages
    """
    # Entires that either break the wiki or are miscategorized
    bad_entries = ["Construction:LKPOINTS WAN", "Profiling"]
    titles = [page["title"] for page in pages]
    for b in bad_entries:
        if b in titles:
            pages.pop(titles.index(b))

    return pages


def _get_incar_tags(html_cache, options_cache):
    """
    Gets the titles text of all pages in the "Category:INCAR tag" category.
    html_json_path is the path to a json file containing the raw HTML of the pages.
    Format is {"tag_name": "html"},
    where tag_name is the actual name of the tag with underscores instead of spaces.

    It takes about 8-10 minutes to generate the raw HTML for all the INCAR tag pages.
    So if you're generating the entire JSON from scratch, you can provide
    the json file to speed things up. It would be quicker to do that than refresh the
    JSON since the wiki isn't updated very regularly.
    """
    pages = _get_category("Category:INCAR tag")

    tags = []
    page_titles = [page["title"] for page in pages]
    pages = _get_from_vasp_wiki(page_titles)
    for p in pages:
        tags.append(_parse_incar_tag(p, html_cache, options_cache))

    return tags


def _get_from_vasp_wiki(title, get_text=True):
    """
    Parse a list of page titles and return a list of dicts with the pageid,
    title, timestamp, and wiki text (if requested).

    Automatically chunks the list into groups of 50 (the max allowed by the API)

    params:
        title: list of page titles
        get_text: whether to get the wiki text or not (default True)
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
                pages.extend(_get_from_vasp_wiki(chunk, get_text=get_text))
            return pages
        title = "|".join(title)  # separate by pipes = list
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


def _get_raw_html(title):
    """
    Gets the HTML of a wiki page using the render action.
    This is practically unstyled HTML that is then styled by
    _tidy_page_html

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


def _get_caches(use_cached_json, use_cached_html, use_cached_options):
    """
    Gets the cached HTML and options for the INCAR tags.

    The cached HTML is pre-generated because it takes a long time to generate and
    VASP wiki is rarely updated.

    The "cached" options are actually hand written summaries because the wiki is not very
    machine-parsable since it's written by/for humans and the formatting of the
    options is not consistent.
    """
    json_cache = {}
    html_cache = {}
    options_cache = {}
    base_db_dir = resources.files("codex.database")

    if use_cached_json:
        json_files = os.listdir(os.path.join(base_db_dir, "json"))
        vasp_versions = [
            match.group(1) for f in json_files for match in [re.match(r"vasp-([0-9]+)", f)] if match
        ]

        if vasp_versions:
            vasp_latest = str(max(map(int, vasp_versions)))
            vasp_latest_date = datetime.fromtimestamp(int(vasp_latest))
            logging.info(
                f"Using cached JSON from {vasp_latest_date} (filename: vasp-{vasp_latest}.json)"
            )
            json_cache_path = os.path.join(base_db_dir, "json", f"vasp-{vasp_latest}.json")
            with open(json_cache_path, "r") as f:
                json_cache = json.load(f)

    # These are the default caches, if they're not there and use cache is True
    # then we want an error
    if use_cached_html:
        html_cache_path = os.path.join(base_db_dir, "vasp-cache", "incar-raw-html.json")
        with open(html_cache_path, "r") as f:
            html_cache = json.load(f)

    if use_cached_options:
        options_cache_path = os.path.join(base_db_dir, "vasp-cache", "incar-options.json")
        with open(options_cache_path, "r") as f:
            options_cache = json.load(f)

    return json_cache, options_cache, html_cache
