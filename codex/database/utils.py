"""
Utilities for the database package.
"""

import re
import logging
import shlex
import subprocess


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


def tidy_dict(d):
    """
    Tidy dicts common in helpdoc's xml output.
    Calls tidy_str on both keys and values.
    """
    tidy_d = {}
    for k, v in d.items():
        if isinstance(k, str):
            k = tidy_str(k)
        if v and isinstance(v, str):
            v = tidy_str(v)
        tidy_d[k] = v
    return tidy_d


def tidy_str(s):
    """
    Tidy strings common in helpdoc's xml output.
    Remove newlines, strip whitespace, remove single quotes.
    """
    s = s.replace("\n", " ").strip()
    if s and s[0] == "'" and s[-1] == "'":
        s = s[1:-1]
    return s


def run_command(command):
    """
    Run a command and return the result. If the command fails, raise an exception.
    """
    logging.info(f"Command: {command}")
    command = shlex.split(command)

    try:
        result = subprocess.run(command, capture_output=True, check=True)
    except subprocess.CalledProcessError as exc:
        logging.error(
            f"Status : FAIL (return code {exc.returncode}),\n"
            f"stdout:\n {exc.stdout},\n"
            f"stderr:\n {exc.stderr}"
        )
        return exc.returncode
    if result.stdout:
        logging.debug(f"Command stdout: {result.stdout.decode('utf-8')}")
    if result.stderr:
        logging.debug(f"Command stderr: {result.stderr.decode('utf-8')}")

    return result.returncode
