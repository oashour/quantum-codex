from flask_smorest import Blueprint

bp_entries = Blueprint(
    "entries",
    "entries",
    url_prefix="/entries",
    description="Operations on individual Codex entries",
)

bp_collections = Blueprint(
    "collections",
    "collections",
    url_prefix="/collections",
    description="Operations on Codex collections",
)

from codex.api import routes
