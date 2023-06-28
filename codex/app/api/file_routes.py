"""
Routes for the /files API endpoint
"""
from flask_smorest import abort
from flask.views import MethodView

from codex.app.api import files_bp as bp
from codex.app.extensions import mongo
from codex.app.api.file_schemas import (
    FileCodexSchema,
    FileCodexQueryArgsSchema,
)

db = mongo.cx["cdx"]["files"]


# TODO: whole thing should require authentication
@bp.route("/")
class FileCodexes(MethodView):
    """End point for FileCodex """

    @bp.arguments(FileCodexQueryArgsSchema, location="query")
    @bp.response(200, FileCodexSchema(many=True))
    def get(self, args):
        """List FileCodex entries"""
        return db.find(args)

    @bp.arguments(FileCodexSchema(many=True), location="json")
    @bp.response(201, FileCodexSchema(many=True))
    def post(self, files):
        """Add a new FileCodex entry"""
        db.insert_many(files)
        return files


@bp.route("/<cdxid>")
class FileCodexById(MethodView):
    """Endpoint for interacting with a Codex entry by ID"""

    @bp.response(200)#, CodexEntrySchema)
    def get(self, cdxid):
        """Get File Codex by ID"""
        file = db.find_one({"_id": cdxid})
        if file is None:
            abort(404, message="Entry not found")
        return file

    # TODO: should require authentication
    @bp.response(204)
    def delete(self, cdxid):
        """Delete FileCodex"""
        result = db.delete_one({"_id": cdxid})
        if result.deleted_count == 0:
            abort(404, message="Entry not found")
        return result.acknowledged
