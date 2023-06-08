"""
Module for dealing with the VASP database (via the wiki)
"""

import requests
import logging

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
                "dimension": 1,
                "optons": {},
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
    Parse a list of page titles and return a list of dicts with the pageid, title, timestamp, and wiki text (if requested).
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
                "optons": {},
                "default": "",
                "info": text,
                "html": None,
                "id": pageid,
                "last_revised": timestamp,
            }
        )
    return pages


def generate_database():
    """
    Generates the database from the wiki.
    """
    database = {}
    pages = get_from_vasp_wiki(EXTRA_PAGE_TITLES, get_text=True)
    database["extras"] = {page["title"]: page for page in pages}
    pages = get_incar_tags(get_text=True)
    database["INCAR"] = {page["title"]: page for page in pages}

    return database


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
