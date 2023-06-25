from flask import render_template, request, flash, redirect, url_for, current_app

from codex.explore import bp
from codex.extensions import inputs, mongo

from codex.models import VaspCodex, EspressoCodex


@bp.route("/explore", methods=["GET", "POST"])
def build_codex():
    if request.method == "POST" and "input_file" in request.files:
        dbversion = request.form["dbversion"]
        code = request.form["code"]
        if code == "VASP":
            Codex = VaspCodex
        elif code == "Quantum ESPRESSO":
            Codex = EspressoCodex

        files = request.files.getlist("input_file")
        codexes = []
        for file in files:
            inputs.save(file)
            file_name = inputs.path(file.filename)
            current_app.logger.info(
                f"Processing input file {file_name} (code: {code}, dbversion: {dbversion}))"
            )
            codexes.append(Codex(file_name, dbversion, mongo.cx))
            flash(f"Input file {file.filename} processed successfully.")

        return render_template("explore.html.j2", codexes=codexes, indent=" " * 2)
    current_app.logger.info(
        f"Got a {request.method} with {len(request.files.getlist('input_file'))} input files."
         "Redirecting to index."
    )
    return redirect(url_for("upload.index"))
