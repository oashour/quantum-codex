"""
Utilities for working with the documentation database
"""
import re
from packaging import version


def get_database(client, code, dbversion):
    """
    Get the database for a specific version of Quantum ESPRESSO from MongoDB

    The databases have names like "code-7x2" and "code-6x4x1"
    (i.e., the version number with periods replaced with "x")
    VASP is the exception: vasp-timestamp where timestamp is seconds since epoch

    This function takes a version string like "7.2 (latest)" or just "latest"
    and returns the correct database name
    """
    database_names = client.list_database_names()

    if code == "vasp":
        available_versions = [
            match.group(1)
            for f in database_names
            for match in [re.match(r"vasp-([0-9]+)", f)]
            if match
        ]
    else:
        available_versions = [
            match.group(1).replace("^", ".")
            for f in database_names
            for match in [re.match(rf"{code}-([0-9]+\^[0-9]+(?:\^[0-9]+)?)", f)]
            if match
        ]

    if code == "vasp" and dbversion.lower() == "latest":
        dbversion = str(max(map(int, available_versions)))
        # vasp_latest_date = datetime.datetime.fromtimestamp(int(vasp_latest))
    elif dbversion.lower() == "latest":
        dbversion = max(available_versions, key=version.parse)
    else:
        match = re.search(r"([0-9]+\.[0-9]+(?:\.[0-9]+)?)", dbversion)
        dbversion = match[1] if match else dbversion

    if dbversion in available_versions:
        return client[f"{code}-{dbversion.replace('.', '^')}"], dbversion
    raise ValueError(f"Version {dbversion} of code {code} not found in database.")
