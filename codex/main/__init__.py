from flask import Blueprint

bp = Blueprint('main', __name__)

from codex.main import routes