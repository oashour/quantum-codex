from flask import Flask, render_template
from codex import Codex
import f90nml
import os
from base64 import b64decode
import json
from importlib import resources
import shutil

def generate_tag_webpages(dir='static', dbversion='7.2'):
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
                if v["html"] != "":#'No documentation was found for this tag.':
                    webpage = b64decode(v["html"]).decode("utf-8")
                    path = os.path.join(tags_dir, p, nl, f"{tag}.html")
                    with open(path, "w") as f:
                        f.write(webpage)

app = Flask(__name__)


@app.route("/")
def hello_world():
    file_name = "example/sr3pbo_bands.in"
    dbversion = 7.2
    codex = Codex(file_name, dbversion)
    generate_tag_webpages(dbversion=dbversion)

    return render_template("input_file.html.j2", codex=codex, indent=" " * 2)
