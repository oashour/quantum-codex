"""
Utilities for the database package.
"""

import re

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

    return t