"""
Routes for the /entries API endpoint
"""
from flask_smorest import abort
from flask.views import MethodView

from codex.api import entries_bp as bp
from codex.extensions import mongo
from codex.api.entry_schemas import (
    CodexEntrySchema,
    CodexEntryQueryArgsSchema,
)

db = mongo.cx["codex"]["entries"]


# TODO: whole thing should require authentication
@bp.route("/")
class CodexEntries(MethodView):
    """End point for Codex entries"""

    @bp.arguments(CodexEntryQueryArgsSchema, location="query")
    @bp.response(200, CodexEntrySchema(many=True))
    def get(self, args):
        """List Codex entries"""
        return db.find(args)

    @bp.arguments(CodexEntrySchema(many=True), location="json")
    @bp.response(201, CodexEntrySchema(many=True))
    def post(self, entries):
        """Add a new Codex entry"""
        db.insert_many(entries)
        return entries


@bp.route("/<uuid>")
class CodexEntryById(MethodView):
    """Endpoint for interacting with a Codex entry by ID"""

    @bp.response(200, CodexEntrySchema)
    def get(self, uuid):
        """Get Codex entry by ID"""
        entry = db.find_one({"_id": uuid})
        if entry is None:
            abort(404, message="Entry not found")
        return entry

    # TODO: should require authentication
    @bp.response(204)
    def delete(self, uuid):
        """Delete Codex entry"""
        result = db.delete_one({"_id": uuid})
        if result.deleted_count == 0:
            abort(404, message="Entry not found")
        return result.acknowledged
