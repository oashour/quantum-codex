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

from codex import STD_CODE_MAP
from codex.main import bp
from codex.extensions import inputs, mongo

from codex.models import CalcCodex, FILE_CODEX_MAP
from codex.api.utils import get_codex, insert_codex
from codex.database.utils import get_database

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


@bp.route("/download/<cdxid>")
def download_codex(cdxid):
    """
    Downloads a Codex from the database and renders it
    """
    calc = get_calc(cdxid, mongo.cx)
    files = [(c["filename"], c["raw_file"]) for c in calc.files]
    memory_file = BytesIO()
    with zipfile.ZipFile(memory_file, "w") as zf:
        for individualFile in files:
            data = zipfile.ZipInfo(individualFile[0])
            data.date_time = time.localtime(time.time())[:6]
            data.compress_type = zipfile.ZIP_DEFLATED
            zf.writestr(data, individualFile[1])
    memory_file.seek(0)

    return send_file(memory_file, download_name=f"{cdxid}.zip", as_attachment=True)
