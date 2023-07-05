"""
Module for the CalcCodex class, which contains a collection of FileCodexes 
represeting a single calculation
"""

# from flask import current_app

from codex.core.utils import generate_cdxid
from codex.core import VaspFileCodex, EspressoFileCodex


FILE_CODEX_MAP = {"vasp": VaspFileCodex, "espresso": EspressoFileCodex}


class CalcCodex:
    """
    Class for a Codex Calculation, i.e., a collection of FileCodexes that
    represent a single calculation
    """

    def __init__(self, code, dbversion, files, **kwargs):
        self._id = kwargs.get("_id", generate_cdxid("calc"))
        self.name = kwargs.get("name") or "Unnamed Calculation"
        self.readme = kwargs.get("readme") or ""
        self.code = code
        self.dbversion = dbversion
        self.files = files

        self.file_ids = [f._id for f in files]

    @classmethod
    def from_files(cls, code, dbversion, files, client, name=None, readme=None):
        """
        Creates a CodexCalc from a list of byte objects
        representing the input files (e.g., from a Flask request)
        """
        Codex = FILE_CODEX_MAP[code]

        # If there's a file called "README.md" or "README.txt", pop it and use it as readme
        for f in files:
            if f.filename.lower() in ["readme.md", "readme.txt"]:
                readme = f.read().decode("utf-8")
                files.remove(f)

        codexes = [Codex.from_file(f, client, dbversion) for f in files]
        dbversion = codexes[0].dbversion  # This is the processed/formatted dbversion
        return CalcCodex(code, dbversion, codexes, name=name, readme=readme)
