from flask import render_template, request, flash, redirect, url_for, current_app, abort

from codex.explore import bp
from codex.extensions import inputs, mongo

from codex.models import CodexCollection
from codex.api.utils import insert_collection, get_collection

db_collections = mongo.cx["codex"]["collections"]
db_entries = mongo.cx["codex"]["entries"]


@bp.route("/explore", methods=["GET", "POST"])
def build_codex():
    if request.method == "POST" and "input_file" in request.files:
        dbversion = request.form["dbversion"]
        code = request.form["code"]

        files = request.files.getlist("input_file")

        collection = CodexCollection.from_files(code, dbversion, files)
        insert_collection(collection, mongo.cx)

        return render_template("explore.html.j2", codexes=collection.entries)
    current_app.logger.info(
        f"Got a {request.method} with {len(request.files.getlist('input_file'))} input files."
        "Redirecting to index."
    )
    return redirect(url_for("upload.index"))

@bp.route("/<cdxid>")
def get_codex(cdxid):
        """
        Gets a CodexCollection from the database and renders it
        """
        collection = get_collection(cdxid, mongo.cx)
        print(collection.entries[0]['dbversion'])

        return render_template("explore.html.j2", codexes=collection.entries)