from flask import render_template, request, flash, redirect, url_for, current_app, abort, request

from codex.main import bp
from codex.extensions import inputs, mongo

from codex.models import CodexCollection
from codex.api.utils import insert_collection, get_collection
from codex.database.utils import get_database

db_collections = mongo.cx["codex"]["collections"]
db_entries = mongo.cx["codex"]["entries"]


@bp.route("/", methods=["GET", "POST"])
def index():
    if request.method == "POST" and "input_file" in request.files:
        dbversion = request.form["dbversion"]
        code = request.form["code"]

        files = request.files.getlist("input_file")

        collection = CodexCollection.from_files(code, dbversion, files)
        insert_collection(collection, mongo.cx)

        return redirect(url_for("main.get_codex", cdxid=collection._id))
    current_app.logger.debug(
        f"Got a {request.method} with {len(request.files.getlist('input_file'))} input files."
        "Redirecting to index."
    )
    current_app.logger.info("Index page loaded.")
    return render_template("upload.html.j2")


@bp.route("/<cdxid>")
def get_codex(cdxid):
    """
    Gets a CodexCollection from the database and renders it
    """
    collection = get_collection(cdxid, mongo.cx)

    return render_template("codex.html.j2", codexes=collection.entries)


# TODO: should this be in the API?
@bp.route("/preview")
def get_preview():
    # TODO: look into context processors instead of some of these tags
    tag_name = request.args.get("tag")
    filetype = request.args.get("filetype")
    section = request.args.get("section")
    dbversion = request.args.get("dbversion")
    code = request.args.get("code")
    current_app.logger.debug(
        f"Generating preview for tag: {tag_name}, (filetype: {filetype}, "
        f"section:{section}, debversion: {dbversion}, code: {code}))"
    )
    db, _ = get_database(mongo.cx, code, dbversion)
    current_app.logger.debug(f"db: {db.name}")

    tag = db[filetype].find_one({"name": tag_name})

    return render_template("preview.html.j2", tag=tag)
