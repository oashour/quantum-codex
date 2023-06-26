"""
Module for the CodexCollection class, which contains a collection of codexes
"""

from flask import current_app

from codex.utils import generate_cdxid
from codex.models import VaspCodex, EspressoCodex
from codex.extensions import mongo


CODEX_MAP = {"vasp": VaspCodex, "espresso": EspressoCodex}
# TODO: this is sort of ugly, maybe an enum?
CODE_MAP = {"VASP": "vasp", "Quantum ESPRESSO": "espresso"}


class CodexCollection:
    """Class for a Codex collection"""

    def __init__(self, code, dbversion, entries, **kwargs):
        print(f"CodexCollection init with {kwargs.get('_id', 'No _id')}")
        self._id = kwargs.get("_id", generate_cdxid())
        print(f"CodexCollection now has {self._id}")
        self.code = code
        self.dbversion = dbversion
        self.entries = entries
        
        # TODO: fix this ugly hack
        # I haven't deserialized the codexes as objects yet...
        if isinstance(entries[0], dict):
            self.entry_ids = [e['_id'] for e in entries]
        else:
            self.entry_ids = [e._id for e in entries]

    
    @classmethod
    def from_files(cls, code, dbversion, files):
        """
        Creates a codex collection from a list of byte objects
        representing the input files (e.g., from a Flask request)
        """
        code = CODE_MAP.get(code, code)
        Codex = CODEX_MAP[code]

        codexes = []
        for f in files:
            current_app.logger.info(
                f"Processing input file {f.filename} (code: {code}, dbversion: {dbversion}))"
            )
            codexes.append(Codex(f, dbversion, mongo.cx))

        dbversion = codexes[0].dbversion  # This is the processed/formatted dbversion
        return CodexCollection(code, dbversion, codexes)
