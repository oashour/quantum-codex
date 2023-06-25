from flask import Blueprint

bp = Blueprint('preview', __name__)

from codex.upload import routes