"""
Module for the VaspCodex class
"""

import logging
from inspect import cleandoc

import f90nml

from codex.core.file_codex import AbstractFileCodex


DOCS_URL = "https://www.quantum-espresso.org/Doc/INPUT_"


class EspressoFileCodex(AbstractFileCodex):
    """
    A class to store the parsed information from a DFT input file
    """

    code = "espresso"
    indent = 2
    comment_token = "! "
    section_start_token = "&"
    section_end_token = "/"

    # TODO: these errors need to go to the webpage?
    def _get_filetype(self, db):
        """
        Figures out what type of input file is being read (pw.x, ph.x, projwfc.x etc)
        """
        nml = f90nml.reads(self.raw_file)
        query = []
        for namelist, tags in nml.items():
            query.extend({"name": tag, "section": namelist} for tag in tags)
        matches = []
        for ft in db.list_collection_names():
            results = list(db[ft].find({"$or": query}))
            if len(results) == len(query):
                matches.append(ft)
        if not matches:
            raise ValueError(f"Could not find file type for {self.filename}")
        if len(matches) > 1:
            logging.warning(f"Found multiple possible file types for {self.filename}: {matches}.")
            logging.warning("Using first file type.")

        return matches[0]

    def _get_tags_cards(self, db):
        """
        Builds a codex for Quantum Espresso, returning HTML
        """
        namelists = f90nml.reads(self.raw_file)
        cards = self.raw_file.split("/")[-1].strip()

        tags = self._get_tags(namelists, db)

        return tags, cards

    def _format_value(self, tag, value):
        """
        Formats a value of a given tag for printing. _file_to_str actually already takes care of this for us so we don't have to do anything
        """
        return value

    @staticmethod
    def _file_to_str(namelist):
        """
        Converts a f90nml.namelist object to a properly formatted string
        """
        nl_str = str(namelist)
        # Strip all lines starting with & or /
        nl_str = "\n".join(
            [
                line
                for line in nl_str.split("\n")
                if not line.startswith("&") and not line.startswith("/") and line.strip()
            ]
        )
        # Replace all .true. with .TRUE. and .false. with .FALSE.
        nl_str = nl_str.replace(".true.", ".TRUE.").replace(".false.", ".FALSE.")
        return cleandoc(nl_str)

    # TODO: need a proper implementation
    # Should redirect to local docs and use tag id to find right section not tag name
    def _get_href(self, tag, db):
        """
        Gets the href to QE docs for a given tag
        """
        return f"{DOCS_URL}{self.filetype.upper()}.html#{tag}"
