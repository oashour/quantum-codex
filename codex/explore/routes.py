from flask import render_template, request, flash, redirect, url_for

from codex.explore import bp
from codex.extensions import inputs, mongo

from codex.models import VaspCodex, EspressoCodex


@bp.route("/explore", methods=["GET", "POST"])
def build_codex():
    print("I made it here")
    print(request.method)
    print(request.files)
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
            print(file_name)
            codexes.append(Codex(file_name, dbversion, mongo.cx))
            flash(f"Input file {file.filename} processed successfully.")

        return render_template("explore.html.j2", codexes=codexes, indent=" " * 2)
    return redirect(url_for("upload.index"))
