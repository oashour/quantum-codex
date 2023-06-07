import wikitextparser as wtp
import requests

API_URL = "https://www.vasp.at/wiki/api.php"
# TODO: include version automatically
USER_AGENT = "dft-codex/0.0.0 (Developer: Omar A. Ashour, ashour@berkeley.edu)"
# API_URL = "https://en.wikipedia.org/w/api.php"


# action=query&generator=categorymembers&gcmtitle=Category:Physics&prop=categories&cllimit=max&gcmlimit=max
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
                "pageid": page["pageid"],
                "title": page["title"],
                "last_revised": page["revisions"][0]["timestamp"],
                "text": None,
            }
        )

    return pages, gcmcontinue


def pull_incar_tags(get_text=True):
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
        pages = parse(page_titles)
    return pages


def parse(title, get_text=True):
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
                pages.extend(parse(chunk, get_text=get_text))
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
            text = wtp.parse(rev["slots"]["main"]["content"])
        pages.append({"pageid": pageid, "title": title, "last_revised": timestamp, "text": text})

    return pages
