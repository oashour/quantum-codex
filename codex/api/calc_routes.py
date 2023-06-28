"""
Routes for the /calc API endpoint
"""
from flask_smorest import abort
from flask.views import MethodView

from codex.api import calcs_bp as bp
from codex.api.utils import insert_calcs
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
        """List CalcCodexes"""
        return list(db.find(args))

    @bp.arguments(CalcCodexSchema(many=True), location="json")
    @bp.response(201)  # , CodexCollectionSchema(many=True))
    def post(self, calcs):
        """Add new CalcCodex"""
        db.insert_many(calcs)
        return calcs


@bp.route("/<cdxid>")
class CalcCodexById(MethodView):
    """Endpoint for interacting with a Calc Codex by CDX-ID"""

    @bp.response(200)
    def get(self, cdxid):
        """Get CalcCodex"""
        calc = db.find_one({"_id": cdxid})
        if calc is None:
            abort(404, message="Collection not found")
        return calc

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
    """Endpoint for creating a CalcCodex from a set of files"""

    @bp.arguments(CalcCodexQueryArgsSchema, location="query")
    @bp.arguments(CalcCodexFilesSchema, location="files")
    @bp.response(201, CalcCodexSchema(exclude=("files",)))
    def post(self, query, files):
        """Add new CalcCodex from files"""
        calc = CalcCodex.from_files(
            query["code"], query["dbversion"], files["input_file"], name=query.get("name")
        )

        insert_calcs(calc, mongo.cx)
        return calc


# @bp.route("/download")
# @bp.response(
#    200, {"format": "binary", "type": "string"}, content_type="application/csv"
# )
# class CodexCollectionDownload(MethodView):
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
# def func():
#    csv_str = "a,b,c\n1,2,3\n4,5,6\n"
#    response = Response(csv_str, mimetype="text/csv")
#    response.headers.set("Content-Disposition", "attachment", filename="file.csv")
#    return response
