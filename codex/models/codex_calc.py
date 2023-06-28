"""
Module for the CalcCodex class, which contains a collection of FileCodexes 
represeting a single calculation
"""

from flask import current_app

from codex.utils import generate_cdxid
from codex.models import VaspFileCodex, EspressoFileCodex
from codex.extensions import mongo


FILE_CODEX_MAP = {"vasp": VaspFileCodex, "espresso": EspressoFileCodex}


class CalcCodex:
    """
    Class for a Codex Calculation, i.e., a collection of FileCodexes that
    represent a single calculation
    """

    def __init__(self, code, dbversion, files, **kwargs):
        self._id = kwargs.get("_id", generate_cdxid("calc"))
        self.code = code
        self.dbversion = dbversion
        self.files = files

        self.file_ids = [f._id for f in files]

    @classmethod
    def from_files(cls, code, dbversion, files):
        """
        Creates a CodexCalc from a list of byte objects
        representing the input files (e.g., from a Flask request)
        """
        Codex = FILE_CODEX_MAP[code]

        codexes = []
        for f in files:
            current_app.logger.info(
                f"Processing input file {f.filename} (code: {code}, dbversion: {dbversion}))"
            )
            codexes.append(Codex.from_file(f, mongo.cx, dbversion))

        dbversion = codexes[0].dbversion  # This is the processed/formatted dbversion
        return CalcCodex(code, dbversion, codexes)
