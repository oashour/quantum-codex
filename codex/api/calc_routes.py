"""
Routes for the /collections API endpoint
"""
from flask_smorest import abort
from flask.views import MethodView

from codex.api import collections_bp as bp
from codex.api.utils import insert_calc
from codex.extensions import mongo

from codex.api.calc_schemas import (
    CalcCodexSchema,
    CalcCodexQueryArgsSchema,
    CalcCodexFilesSchema,
)

from codex.models import CalcCodex

db = mongo.cx["cdx"]["calcs"]


# TODO: whole endpoint should require authentication
@bp.route("/")
class CalcCodexes(MethodView):
    """Endpoint for interacting with Calc Codexes"""

    @bp.arguments(
        CalcCodexQueryArgsSchema(
            partial=(
                "code",
                "dbversion",
            )
        ),
        location="query",
    )
    @bp.response(200)  # , CodexCollectionSchema(many=True))
    def get(self, args):
        """List Codex collections"""
        return list(db.find(args))

    @bp.arguments(CalcCodexSchema(many=True), location="json")
    @bp.response(201)  # , CodexCollectionSchema(many=True))
    def post(self, entries):
        """Add new Codex collection"""
        db.insert_many(entries)
        return entries


@bp.route("/<cdxid>")
class CalcCodexById(MethodView):
    """Endpoint for interacting with a Calc Codex by CDX-ID"""

    @bp.response(200)
    def get(self, cdxid):
        """Get CalcCodex"""
        collection = db.find_one({"_id": cdxid})
        if collection is None:
            abort(404, message="Collection not found")
        return collection

    # TODO: should require authentication
    @bp.response(204)
    def delete(self, cdxid):
        """Delete CalcCodex"""
        result = db.delete_one({"_id": cdxid})
        if result.deleted_count == 0:
            abort(404, message="Entry not found")
        return result.acknowledged


@bp.route("/fromfile")
class CalcCodexFromFiles(MethodView):
    """Endpoint for creating Codex collections from a set of files"""

    @bp.arguments(CalcCodexQueryArgsSchema, location="query")
    @bp.arguments(CalcCodexFilesSchema, location="files")
    @bp.response(201, CalcCodexSchema(exclude=("entries",)))
    def post(self, query, files):
        """Add new CalcCodex from files"""
        # print(data, files)
        collection = CalcCodex.from_files(
            query["code"], query["dbversion"], files["input_file"]
        )

        insert_calc(collection, mongo.cx)
        return collection

#@bp.route("/download")
#@bp.response(
#    200, {"format": "binary", "type": "string"}, content_type="application/csv"
#)
#class CodexCollectionDownload(MethodView):
#    """Endpoint for creating Codex collections from a set of files"""
#
#    @bp.arguments(CodexCollectionQueryArgsSchema, location="query")
#    @bp.arguments(CodexCollectionFilesSchema, location="files")
#    @bp.response(201, CodexCollectionSchema(exclude=("entries",)))
#    def post(self, query, files):
#        """Add new Codex collection from files"""
#        # print(data, files)
#        collection = CodexCollection.from_files(
#            query["code"], query["dbversion"], files["input_file"]
#        )
#
#        insert_collection(collection, mongo.cx)
#        return collection
#def func():
#    csv_str = "a,b,c\n1,2,3\n4,5,6\n"
#    response = Response(csv_str, mimetype="text/csv")
#    response.headers.set("Content-Disposition", "attachment", filename="file.csv")
#    return response