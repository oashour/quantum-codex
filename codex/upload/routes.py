from flask import render_template, request, flash

from codex.upload import bp
from codex.extensions import inputs

from codex import Codex

@bp.route("/")
def index():
    return render_template("upload.html.j2")
