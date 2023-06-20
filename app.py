"""
Main CODEX flask web app
"""

import os
from base64 import b64decode
import json
from importlib import resources
import shutil

from flask import Flask, flash, request, redirect, url_for, render_template
from flask_uploads import IMAGES, UploadSet, configure_uploads
from werkzeug.utils import secure_filename
import f90nml

from codex import Codex

app = Flask(__name__)
app.jinja_env.lstrip_blocks = True
app.jinja_env.trim_blocks = True


@app.template_filter("isinstance")
def is_instance(value, t):
    return isinstance(value, eval(t))


# TODO: better checking
inputs = UploadSet("inputs", ["pwi", "in", "vasp"])
app.config["UPLOADED_INPUTS_DEST"] = "static/input_files"
app.config["SECRET_KEY"] = os.urandom(24)
configure_uploads(app, inputs)


def get_latest_db_version():
    base_db_dir = resources.files("codex.database")
    db_versions = [
        f for f in os.listdir(base_db_dir) if os.path.isdir(os.path.join(base_db_dir, f))
    ]
    db_versions = [
        f.split("-")[1]
        for f in db_versions
        if f.split("-")[0] == "vasp" and f.split("-")[1].isdigit()
    ]
    return sorted(db_versions)[-1]


@app.route("/", methods=["GET", "POST"])
def upload():
    if request.method == "POST" and "input_file" in request.files:
        dbversion = request.form["dbversion"].replace(" (latest)", "")
        code = request.form["code"]
        if code.lower() == "vasp":
            dbversion = get_latest_db_version()

        files = request.files.getlist("input_file")
        codexes = []
        for file in files:
            inputs.save(file)
            file_name = inputs.path(file.filename)
            codexes.append(Codex(file_name, code, dbversion))
            flash(f"Input file {file.filename} processed successfully.")

        return render_template("codex.html.j2", codexes=codexes, indent=" " * 2)
    return render_template("upload.html.j2")


@app.route("/get_preview")
def get_preview():
    # TODO: look into context processors instead of some of these tags
    tag_name = request.args.get("tag")
    filetype = request.args.get("filetype")
    section = request.args.get("section")
    dbversion = request.args.get("dbversion")
    code = request.args.get("code")

    base_db_dir = resources.files("codex.database")
    database_dir = os.path.join(base_db_dir, f"{code}-{dbversion}")
    database_filename = os.path.join(database_dir, "database.json")

    with open(database_filename) as f:
        database = json.load(f)
    if code == "vasp":
        tag = database[filetype][tag_name]
    else:
        tag = database[filetype][section][tag_name]

    return render_template("tag.html.j2", tag=tag, tag_name=tag_name)
