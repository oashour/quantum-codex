"""
Blueprints for the API
"""
from flask_smorest import Blueprint

files_bp = Blueprint(
    "files",
    "files",
    url_prefix="/files",
    description="Operations on FileCodexes",
)

calcs_bp = Blueprint(
    "calcs",
    "calcs",
    url_prefix="/calcs",
    description="Operations on CalcCodexes",
)


from codex.api import calc_routes, file_routes
