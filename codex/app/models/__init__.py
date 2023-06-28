"""
Models for Codex
"""

from codex.app.models.file_codex import CodexTag, AbstractFileCodex
from codex.app.models.vasp import VaspFileCodex
from codex.app.models.espresso import EspressoFileCodex
from codex.app.models.codex_calc import CalcCodex, FILE_CODEX_MAP
