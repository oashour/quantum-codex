"""
Module for the CodexCollection class, which contains a collection of codexes
"""

from flask import current_app

from codex.utils import generate_cdxid
from codex.models import VaspCodex, EspressoCodex
from codex.extensions import mongo


CODEX_MAP = {"vasp": VaspCodex, "espresso": EspressoCodex}


class CodexCollection:
    """Class for a Codex collection"""

    def __init__(self, code, dbversion, entries):
        self._id = generate_cdxid()
        self.code = code
        self.dbversion = dbversion
        self.entries = entries
        self.entry_ids = [e._id for e in entries]

    @classmethod
    def from_files(cls, code, dbversion, files):
        """
        Creates a codex collection from a list of byte objects
        representing the input files (e.g., from a Flask request)
        """
        Codex = CODEX_MAP[code]

        codexes = []
        for f in files:
            current_app.logger.info(
                f"Processing input file {f.filename} (code: {code}, dbversion: {dbversion}))"
            )
            codexes.append(Codex(f, dbversion, mongo.cx))

        dbversion = codexes[0].dbversion  # This is the processed/formatted dbversion
        return CodexCollection(code, dbversion, codexes)
