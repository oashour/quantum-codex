"""
Module for the VaspCodex class
"""

import os
from urllib.parse import quote
import re
import tempfile

from pymatgen.io.vasp.inputs import Incar, Poscar, Potcar, Kpoints

from codex.models.file_codex import AbstractFileCodex

WIKI_URL = "https://www.vasp.at/wiki/index.php"


class VaspFileCodex(AbstractFileCodex):
    """
    A class to store the parsed information from a DFT input file
    """

    code = "vasp"
    indent = 0
    comment_token = "! "
    section_start_token = ""
    section_end_token = ""

    # TODO: these errors need to go to the webpage?
    def _get_filetype(self, db):
        """
        Figures out what type of input file is being read (INCAR vs POSCAR vs KPOINTS vs POTCAR)
        """
        match = re.match(r"^(INCAR|POSCAR|KPOINTS|POTCAR)", self.filename)
        if match is None:
            raise ValueError(
                f"Could not find file type for {self.filename}. I can only read INCAR, "
                "POSCAR, KPOINTS, and POTCAR files, and their names need to start that way,"
                "e.g., INCAR, INCAR-Si, INCARSi, INCAR_Si, etc. are all valid"
            )
        return match[1]

    def _get_tags_cards(self, db):
        """
        Builds a codex for Quantum Espresso, returning HTML
        """
        if self.filetype == "INCAR":
            incar = Incar.from_string(self.raw_file)
            # This avoids some bugs
            incar = Incar({k.upper(): v for k, v in incar.items()})
            # Passes the incar with a fake 'section', since it's an unsectioned file
            tags = self._get_tags({"": incar}, db)
            cards = ""
        elif self.filetype == "POSCAR":
            # TODO: add proper POSCAR support
            poscar = Poscar.from_string(self.raw_file)
            tags = {}
            cards = poscar.get_string()
        elif self.filetype == "KPOINTS":
            # TODO: add proper KPOINTS support
            kpoints = Kpoints.from_string(self.raw_file)
            tags = {}
            cards = str(kpoints)
        elif self.filetype == "POTCAR":
            # TODO: add proper POTCAR support
            # Potcar doesn't have a from_string method, so we use a tempfile
            with tempfile.NamedTemporaryFile() as tmp:
                with open(tmp.name, 'w') as f:
                    f.write(self.raw_file)
                potcar = Potcar.from_file(tmp.name)
            tags = {}
            cards = str(potcar)

        return tags, cards

    def _format_value(self, tag, value):
        """
        Formats a value of a given tag for printing
        """
        incar_string = f"{tag} = {value}"
        incar = Incar.from_string(incar_string)
        formatted_value = incar.get_string(sort_keys=True).split("=")[1].strip()
        formatted_value = formatted_value.replace("True", ".TRUE.")
        formatted_value = formatted_value.replace("False", ".FALSE.")
        return formatted_value

    @staticmethod
    def _file_to_str(vasp_input_file):
        """
        Converts a VASP input file to a string
        Takes a dictionary of the form {"": Incar} or {"": Poscar} etc.
        The "" is the section name, which is not used for VASP
        """
        return vasp_input_file[""].get_string()

    def _get_href(self, tag, db):
        """
        Gets the href to VASP wiki for an INCAR tag
        """
        return f"{WIKI_URL}/{quote(tag)}"
