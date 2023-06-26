"""
Module for the CodexCollection class, which contains a collection of codexes
"""

from codex.utils import generate_cdxid

class CodexCollection:
    """Class for a Codex collection"""
    def __init__(self, code, dbversion, codexes):
        self.cdxid = generate_cdxid()
        self.code = code
        self.dbversion = dbversion
        self.entries = [c["_id"] for c in codexes]