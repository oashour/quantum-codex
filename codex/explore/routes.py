from flask import render_template, request, flash

from codex.explore import bp
from codex.extensions import inputs

from codex import Codex

@bp.route("/explore", methods=["GET", "POST"])
def build_codex():
    if request.method == "POST" and "input_file" in request.files:
        dbversion = request.form["dbversion"]
        code = request.form["code"]

        files = request.files.getlist("input_file")
        codexes = []
        for file in files:
            inputs.save(file)
            file_name = inputs.path(file.filename)
            codexes.append(Codex(file_name, code, dbversion))
            flash(f"Input file {file.filename} processed successfully.")

        return render_template("explore.html.j2", codexes=codexes, indent=" " * 2)