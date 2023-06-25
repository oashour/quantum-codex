"""
Module for the VaspCodex class
"""

import os
from urllib.parse import quote
import re

from pymatgen.io.vasp.inputs import Incar, Poscar, Potcar, Kpoints
from codex.models.codex import AbstractCodex

WIKI_URL = "https://www.vasp.at/wiki/index.php"


class VaspCodex(AbstractCodex):
    """
    A class to store the parsed information from a DFT input file
    """

    code = "vasp"
    indent = 0 * " "
    comment_token = "! "
    section_start_token = ""
    section_end_token = ""
    code_pretty = "VASP"

    # TODO: these errors need to go to the webpage?
    def _get_filetype(self, filename, db):
        """
        Figures out what type of input file is being read (INCAR vs POSCAR vs KPOINTS vs POTCAR)
        """
        basename = os.path.basename(filename)
        match = re.match(r"^(INCAR|POSCAR|KPOINTS|POTCAR)", basename)
        if match is None:
            raise ValueError(
                f"Could not find package for {basename}. I can only read INCAR, POSCAR,"
                "KPOINTS, and POTCAR files, and their names need to start that way,"
                "e.g., INCAR, INCAR-Si, INCARSi, INCAR_Si, etc. are all valid"
            )
        filetype = match.group(1)
        return filetype

    def _get_tags_cards(self, input_filename, db):
        """
        Builds a codex for Quantum Espresso, returning HTML
        """
        if self.filetype == "INCAR":
            incar = Incar.from_file(input_filename)
            # This avoids some bugs
            incar = Incar({k.upper(): v for k, v in incar.items()})
            # Passes the incar with a fake 'section', since it's an unsectioned file
            tags = self._get_tags({"": incar}, db)
            cards = ""
        elif self.filetype == "POSCAR":
            # TODO: add proper POSCAR support
            poscar = Poscar.from_file(input_filename)
            tags = {}
            cards = poscar.get_string()
        elif self.filetype == "KPOINTS":
            # TODO: add proper KPOINTS support
            kpoints = Kpoints.from_file(input_filename)
            tags = {}
            cards = str(kpoints)
        elif self.filetype == "POTCAR":
            # TODO: add proper POTCAR support
            potcar = Potcar.from_file(input_filename)
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
    def _file_to_str(file):
        return file[""].get_string()

    def _get_href(self, tag, db):
        """
        Gets the href to VASP wiki for an INCAR tag
        """
        return WIKI_URL + "/" + quote(tag)
