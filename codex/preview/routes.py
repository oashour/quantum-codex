from flask import render_template, request, flash

from codex.preview import bp
from codex.extensions import mongo
from codex.database.utils import get_database

from codex import Codex


@bp.route("/preview")
def get_preview():
    # TODO: look into context processors instead of some of these tags
    tag_name = request.args.get("tag")
    filetype = request.args.get("filetype")
    section = request.args.get("section")
    dbversion = request.args.get("dbversion")
    code = request.args.get("code")
    db = get_database(mongo.cx, code, dbversion)[filetype]

    tag = db.find_one({"name": tag_name})

    return render_template("preview.html.j2", tag=tag)
