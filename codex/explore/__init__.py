from flask import Blueprint

bp = Blueprint('explore', __name__)

from codex.explore import routes