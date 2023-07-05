"""
Models module for the Postgres database
"""
from sqlalchemy.inspection import inspect

from codex.app.models.file import FileCodexModel
from codex.app.models.tag import TagModel
from codex.app.models.calc import CalcCodexModel

CODEX_MODELS = {'file': FileCodexModel, 'calc': CalcCodexModel}
