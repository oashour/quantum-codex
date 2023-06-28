from flask_smorest import Blueprint

entries_bp = Blueprint(
    "entries",
    "entries",
    url_prefix="/entries",
    description="Operations on individual Codex entries",
)

collections_bp = Blueprint(
    "collections",
    "collections",
    url_prefix="/collections",
    description="Operations on Codex collections",
)


from codex.api import calc_routes, file_routes
