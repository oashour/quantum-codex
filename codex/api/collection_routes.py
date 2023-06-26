"""
Routes for the /collections API endpoint
"""
from flask_smorest import abort
from flask.views import MethodView

from codex.api import collections_bp as bp
from codex.extensions import api, mongo

from codex.api.entry_schemas import CodexEntrySchema
from codex.api.collection_schemas import (
    CodexCollectionSchema,
    CodexCollectionQueryArgsSchema,
    CodexCollectionFilesArgsSchema,
    CodexCollectionFilesSchema,
)

from codex.models import CodexCollection

# from codex.models import Codex

db = mongo.cx["codex"]["collections"]
db_entries = mongo.cx["codex"]["entries"]


# TODO: whole endpoint should require authentication
@bp.route("/")
class CodexCollections(MethodView):
    """Endpoint for interacting with Codex collections"""

    @bp.arguments(
        CodexCollectionQueryArgsSchema(
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

    @bp.arguments(CodexCollectionSchema(many=True), location="json")
    @bp.response(201)  # , CodexCollectionSchema(many=True))
    def post(self, entries):
        """Add new Codex collection"""
        db.insert_many(entries)
        return entries


@bp.route("/<cdxid>")
class CodexCollectionById(MethodView):
    """Endpoint for interacting with a Codex collection by CDX-ID"""

    @bp.response(200)
    def get(self, cdxid):
        """Get Codex collection by CDX-ID"""
        collection = db.find_one({"_id": cdxid})
        if collection is None:
            abort(404, message="Collection not found")
        return collection

    # TODO: should require authentication
    @bp.response(204)
    def delete(self, cdxid):
        """Delete Codex collection"""
        result = db.delete_one({"_id": cdxid})
        if result.deleted_count == 0:
            abort(404, message="Entry not found")
        return result.acknowledged


@bp.route("/fromfile")
class CodexCollectionFromFile(MethodView):
    """Endpoint for creating Codex collections from a set of files"""

    @bp.arguments(CodexCollectionQueryArgsSchema, location="query")
    @bp.arguments(CodexCollectionFilesSchema, location="files")
    @bp.response(201, CodexCollectionSchema(exclude=("entries",)))
    def post(self, query, files):
        """Add new Codex collection from files"""
        # print(data, files)
        collection = CodexCollection.from_files(
            query["code"], query["dbversion"], files["input_file"]
        )

        db_entries.insert_many(CodexEntrySchema().dump(collection.entries, many=True))
        db.insert_one(CodexCollectionSchema(exclude=("entries",)).dump(collection))

        return collection
