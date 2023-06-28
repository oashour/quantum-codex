"""
Routes for the main application
"""
import zipfile
import time
from io import BytesIO

from flask import (
    render_template,
    request,
    flash,
    redirect,
    url_for,
    current_app,
    abort,
    request,
    send_file,
)

from codex.app import STD_CODE_MAP
from codex.app.main import bp
from codex.app.extensions import inputs, mongo

from codex.app.models import CalcCodex, FILE_CODEX_MAP
from codex.app.db_utils import get_codex, insert_codex
from codex.app.docdb_utils import get_database

from codex.utils import get_type_from_cdxid

db_calcs = mongo.cx["cdx"]["calcs"]
db_files = mongo.cx["cdx"]["files"]


@bp.route("/", methods=["GET", "POST"])
def index():
    """Index page"""
    current_app.logger.info("Index page loaded.")
    return render_template("upload.html.j2")


@bp.route("/build_codex", methods=["GET", "POST"])
def build_codex():
    """
    Builds a Codex from the files uploaded by the user
    """
    if request.method == "POST" and "input_file" in request.files:
        dbversion = request.form["dbversion"]
        code = request.form["code"]
        name = request.form.get("codex-name")
        code = STD_CODE_MAP.get(code, code)

        files = request.files.getlist("input_file")

        if len(files) == 1:
            Codex = FILE_CODEX_MAP[code]
            codex = Codex.from_file(files[0], mongo.cx, dbversion)
            print(f"I'm here with {codex._id}")
        else:
            codex = CalcCodex.from_files(code, dbversion, files, name=name)

        insert_codex(codex, mongo.cx)

        return redirect(url_for("main.get_codex_by_id", cdxid=codex._id))
    if request.method == "GET" and "cdxid" in request.args:
        cdxid = request.args.get("cdxid")
        return redirect(url_for("main.get_codex_by_id", cdxid=cdxid))


@bp.route("/<cdxid>")
def get_codex_by_id(cdxid):
    """
    Gets a CodexCollection from the database and renders it
    """
    codex, codex_type = get_codex(cdxid, mongo.cx)

    return render_template("codex.html.j2", codex=codex, codex_type=codex_type)


# TODO: should this be in the API?
@bp.route("/preview")
def get_preview():
    """
    Renders a preview for a given tag
    """
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


# TODO: this needs a lot of work:
# 1. pass boolean arguments
# 2. refactor for refuse in API
# 3. Support for README and ProjCodex
@bp.route("/download")
def download_codex():
    """
    Downloads a Codex from the database and renders it
    """
    cdxid = request.args["cdxid"]
    raw = request.args.get("format") == "raw"
    pretty = "pretty" in request.args
    with_comments = "comments" in request.args
    print(request.args)
    codex, codex_type = get_codex(cdxid, mongo.cx)
    if codex_type == "file":
        return send_file(
            BytesIO(codex.to_string(raw, pretty, with_comments).encode("utf-8")),
            download_name=codex.filename,
            as_attachment=True,
        )
    elif codex_type == "calc":
        files = [(c.filename, c.to_string(raw, pretty, with_comments)) for c in codex.files]
        memory_file = BytesIO()
        with zipfile.ZipFile(memory_file, "w") as zf:
            for individualFile in files:
                data = zipfile.ZipInfo(individualFile[0])
                data.date_time = time.localtime(time.time())[:6]
                data.compress_type = zipfile.ZIP_DEFLATED
                zf.writestr(data, individualFile[1])
        memory_file.seek(0)

        return send_file(memory_file, download_name=f"{cdxid}.zip", as_attachment=True)
