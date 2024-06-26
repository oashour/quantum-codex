"""
Utility functions shared betwen the app, database generation and CLI
"""

import re
import operator

import nanoid
from lxml import etree

CDX_ID_ALPHABET = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
CDX_ID_LENGTH = 12
CDX_ID_REGEX = rf"cdx-([fcp])-[{CDX_ID_ALPHABET}]{{{CDX_ID_LENGTH}}}"
CDX_ID_TYPE_MAP = {"f": "file", "c": "calc", "p": "project"}


def remove_html_tags(text):
    """Remove html tags from a string"""
    if text:
        parser = etree.HTMLParser()
        tree = etree.fromstring(text, parser)
        string = etree.tostring(tree, encoding="unicode", method="text")
        return string.strip()
    return text


def get_type_from_cdxid(cdxid):
    """
    Gets codex type from the ID
    """
    validate_cdxid(cdxid)
    return CDX_ID_TYPE_MAP[cdxid.split("-")[1]]


def generate_cdxid(cdx_type):
    """
    Generates a random cdx id using nanoid
    """
    if cdx_type in ("file", "calc", "project"):
        x = cdx_type[0]
    else:
        raise ValueError(f"Invalid cdx type: {cdx_type}")
    return f"cdx-{x}-{nanoid.generate(CDX_ID_ALPHABET, size=12)}"


def validate_cdxid(cdxid):
    """
    Validates a cdx id. These are of the form cdx-[fcp]-<12 alphanumeric characters>
    The alpha numeric characters can be 0-9, A-Z, or a-z
    """
    if not re.match(CDX_ID_REGEX, cdxid):
        raise ValueError("Invalid cdxid")


def range_dict_get(tag, range_dict):
    """
    Finds the value corresponding to a key (`tag`) in a dictionary whose keys
    might include ranges. The dictionary keys are either just the `tag` itself
    (for discrete values) or a string describing a range (for continuous values).

    Expects `range_dict` to have string keys and values (i.e., json)

    Realistic Example:
    ```
    options = {'-5': 'Tetrahedron method (Blochl Corrections)'
    '-4': 'Tetrahedron method'
    '-3': 'MP method'
    '-2': 'Partial Occupancies'
    '-1': 'Fermi smearing'
    '0': 'Gaussian smearing'
    '[integer]>0': 'MP method of order {}'
    }
    get_from_range_dict(-5, options) # returns 'Tetrahedron method (Blochl Corrections)'
    get_from_range_dict(2, options) # returns 'MP method of order 2'
    ```

    Convoluted example (move to unit test):

    weather_status = {
        "[integer]<=      -10": "It's freezing! It's {} degrees outside",
        "20": "Weather is okay.",
        "[float]    >+50.0": "It's boiling! It's {} degrees outside.",
    }
    range_dict_get(20, weather_status)    # returns "Weather is okay."
    range_dict_get(21, weather_status)    # returns None
    range_dict_get(50.0, weather_status)  # returns None
    range_dict_get(60.3, weather_status)  # returns "It's boiling! It's 60.3 degrees outside."
    range_dict_get(-20, weather_status)   # returns "It's freezing! It's -20 degrees outside."
    range_dict_get(-30.0, weather_status) # returns "It's freezing! It's -30 degrees outside."
    range_dict_get(-50.2, weather_status) # returns None
    """
    # See if it's one of the discrete values
    val = range_dict.get(str(tag.lower()))
    if val is not None:
        return val

    # If not, see if it's a range
    pattern = r"\[(\w+)\]\s*([><]=?)\s*([+-]?\d+.?\d+)"
    for k in range_dict.keys():
        if match := re.match(pattern, k):
            datatype = float if match[1].lower() == "float" else int
            limit = datatype(match[3])

            comp = {"<": operator.lt, "<=": operator.le, ">": operator.gt, ">=": operator.ge}
            comp = comp[match[2]]

            # We don't want strong typing here, so we'll allow some flexibility
            # Integer value matches float range (e.g., 50 will match [FLOAT] <= 50)
            # Float value matches integer range if it's actually an integer
            # (e.g., 50.0 will match [INTEGER] <= 50)
            if (
                isinstance(tag, datatype)
                or (isinstance(tag, int) and datatype == float)
                or (isinstance(tag, float) and datatype == int and tag.is_integer())
            ) and comp(tag, limit):
                return range_dict[k].format(datatype(tag))

    return None
