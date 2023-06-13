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


def generate_tag_webpages(dir="static", dbversion="7.2"):
    base_db_dir = resources.files("codex.database")
    code = "qe"  # Need better terminology
    database_dir = os.path.join(base_db_dir, f"{code}-{dbversion}")
    database_filename = os.path.join(database_dir, "database.json")

    with open(database_filename) as f:
        database = json.load(f)

    working_dir = os.path.abspath(dir)
    tags_dir = os.path.join(working_dir, "tags-qe")
    if os.path.exists(tags_dir):
        shutil.rmtree(tags_dir, ignore_errors=True)
    os.mkdir(tags_dir)
    for p, package in database.items():
        os.mkdir(os.path.join(tags_dir, p))
        for nl, namelist in package.items():
            os.mkdir(os.path.join(tags_dir, p, nl))
            for tag, v in namelist.items():
                if v["html"] != "":  #'No documentation was found for this tag.':
                    webpage = b64decode(v["html"]).decode("utf-8")
                    path = os.path.join(tags_dir, p, nl, f"{tag}.html")
                    with open(path, "w") as f:
                        f.write(webpage)


app = Flask(__name__)
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
        generate_tag_webpages(dbversion=dbversion)
        code = request.form["code"]

        files = request.files.getlist("input_file")
        codexes = []
        for file in files:
            inputs.save(file)
            file_name = inputs.path(file.filename)
            codexes.append(Codex(file_name, code, dbversion))
            flash(f"Input file {file.filename} processed successfully.")

        return render_template("codex.html.j2", codexes=codexes, indent=" " * 2)
    return render_template("upload.html.j2")
