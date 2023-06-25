from flask import render_template, request, current_app

from codex.preview import bp
from codex.extensions import mongo
from codex.database.utils import get_database


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
