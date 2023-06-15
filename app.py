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


@app.route("/", methods=["GET", "POST"])
def upload():
    # if request.method == "GET":
    #    file_name = "example/sr3pbo_bands.in"
    #    dbversion = 7.2
    #    codex = Codex(file_name, dbversion)
    #    generate_tag_webpages(dbversion=dbversion)

    #    return render_template("input_file.html.j2", codex=codex, indent=" " * 2)
    if request.method == "POST" and "input_file" in request.files:

        dbversion = request.form["dbversion"].replace(" (latest)", "")
        code = request.form["code"]
        dbversion = "1686736265" if code == "VASP" else dbversion
        #generate_tag_webpages(dbversion=dbversion.lower(), code=code)

        files = request.files.getlist("input_file")
        codexes = []
        for file in files:
            inputs.save(file)
            file_name = inputs.path(file.filename)
            codexes.append(Codex(file_name, code, dbversion))
            flash(f"Input file {file.filename} processed successfully.")

        return render_template("codex.html.j2", codexes=codexes, indent=" " * 2)
    return render_template("upload.html.j2")

@app.route('/get_preview')
def get_preview():
    # TODO: look into context processors instead of some of these tags
    tag = request.args.get('tag')
    filetype = request.args.get('filetype')
    section = request.args.get('section')
    dbversion = request.args.get('dbversion')
    code = request.args.get('code')
    print(tag, filetype, section, dbversion, code)

    base_db_dir = resources.files("codex.database")
    database_dir = os.path.join(base_db_dir, f"{code}-{dbversion}")
    database_filename = os.path.join(database_dir, "database.json")

    with open(database_filename) as f:
        database = json.load(f)
    if code == "vasp":
        tag = database[filetype][tag]
    else:
        tag = database[filetype][section][tag]

    return render_template('tag.html.j2', tag=tag)
