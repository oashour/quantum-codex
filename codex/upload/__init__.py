from flask import Blueprint

bp = Blueprint('upload', __name__)

from codex.upload import routes