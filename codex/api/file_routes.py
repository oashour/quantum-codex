"""
Routes for the /entries API endpoint
"""
from flask_smorest import abort
from flask.views import MethodView

from codex.api import entries_bp as bp
from codex.extensions import mongo
from codex.api.file_schemas import (
    FileCodexSchema,
    FileCodexQueryArgsSchema,
)

db = mongo.cx["cdx"]["files"]


# TODO: whole thing should require authentication
@bp.route("/")
class FileCodices(MethodView):
    """End point for FileCodex """

    @bp.arguments(FileCodexQueryArgsSchema, location="query")
    @bp.response(200, FileCodexSchema(many=True))
    def get(self, args):
        """List FileCodex entries"""
        return db.find(args)

    @bp.arguments(FileCodexSchema(many=True), location="json")
    @bp.response(201, FileCodexSchema(many=True))
    def post(self, entries):
        """Add a new Codex entry"""
        db.insert_many(entries)
        return entries


@bp.route("/<uuid>")
class FileCodexById(MethodView):
    """Endpoint for interacting with a Codex entry by ID"""

    @bp.response(200)#, CodexEntrySchema)
    def get(self, uuid):
        """Get File Codex by ID"""
        entry = db.find_one({"_id": uuid})
        if entry is None:
            abort(404, message="Entry not found")
        return entry

    # TODO: should require authentication
    @bp.response(204)
    def delete(self, uuid):
        """Delete FileCodex"""
        result = db.delete_one({"_id": uuid})
        if result.deleted_count == 0:
            abort(404, message="Entry not found")
        return result.acknowledged
