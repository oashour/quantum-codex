from flask import render_template, current_app

from codex.upload import bp
from codex.extensions import inputs

@bp.route("/")
def index():
    current_app.logger.info("Index page loaded.")
    return render_template("upload.html.j2")
