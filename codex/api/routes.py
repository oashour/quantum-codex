from flask_smorest import abort
from flask.views import MethodView
from flask import current_app

from codex.api import bp_collections as bpc
from codex.api import bp_entries as bpe
from codex.extensions import api, mongo
from codex.api.schemas import (
    CodexEntrySchema,
    CodexEntryQueryArgsSchema,
    CodexCollectionSchema,
    CodexCollectionQueryArgsSchema,
    CodexCollectionQueryFilesSchema,
)
from codex.models import VaspCodex, EspressoCodex, CodexCollection

# from codex.models import Codex

db_collections = mongo.cx["codex"]["collections"]
db_entries = mongo.cx["codex"]["entries"]


# TODO: should require authentication
@bpe.route("/")
class CodexEntries(MethodView):
    @bpe.arguments(CodexEntryQueryArgsSchema, location="query")
    @bpe.response(200, CodexEntrySchema(many=True))
    def get(self, args):
        """List Codex entries"""
        return db_entries.find(args)

    @bpe.arguments(CodexEntrySchema(many=True), location="json")
    @bpe.response(201, CodexEntrySchema(many=True))
    def post(self, entries):
        """Add a new Codex entry"""
        db_entries.insert_many(entries)
        return entries


@bpe.route("/<codex_entry_id>")
class CodexEntryById(MethodView):
    @bpe.response(200, CodexEntrySchema)
    def get(self, codex_entry_id):
        """Get Codex entry by ID"""
        entry = db_entries.find_one({"_id": codex_entry_id})
        if entry is None:
            abort(404, message="Entry not found")
        return entry

    # TODO: should require authentication
    @bpe.response(204)
    def delete(self, codex_entry_id):
        """Delete Codex collection"""
        result = db_entries.delete_one({"_id": codex_entry_id})
        if result.deleted_count == 0:
            abort(404, message="Entry not found")
        return result.acknowledged


# TODO: should require authentication
@bpc.route("/")
class CodexCollections(MethodView):
    @bpc.arguments(CodexCollectionQueryArgsSchema, location="query")
    @bpc.response(200, CodexCollectionSchema(many=True))
    def get(self, args):
        """List Codex collections"""
        return db_collections.find(args)

    @bpc.arguments(CodexCollectionSchema(many=True), location="json")
    @bpc.response(201, CodexCollectionSchema(many=True))
    def post(self, entries):
        """Add new Codex collection"""
        db_collections.insert_many(entries)
        return entries


@bpc.route("/fromfile")
class CodexCollectionFromFile(MethodView):
    @bpc.arguments(CodexCollectionQueryFilesSchema, location="json")
    @bpc.response(201, CodexCollectionSchema)
    def post(self, data):
        """Add new Codex collection from files"""
        dbversion = data["dbversion"]
        code = data["code"]
        # TODO: move to masp in codex.models.__init__
        if code == "vasp":
            Codex = VaspCodex
        elif code == "espresso":
            Codex = EspressoCodex

        files = data["files"]
        codexes = []
        for file in files:
            # inputs.save(file)
            file_name = file.filename
            current_app.logger.info(
                f"Processing input file {file_name} (code: {code}, dbversion: {dbversion}))"
            )
            codexes.append(Codex(file_name, dbversion, mongo.cx))
            collection = CodexCollection(code, dbversion, codexes)
            # TODO: send post to /collections to add to DB
        return collection


@bpc.route("/<cdxid>")
class CodexCollectionById(MethodView):
    @bpc.response(200, CodexCollectionSchema)
    def get(self, cdxid):
        """Get Codex collection by CDX-ID"""
        collection = db_entries.find_one({"_id": cdxid})
        if collection is None:
            abort(404, message="Collection not found")
        return collection

    # TODO: should require authentication
    @bpe.response(204)
    def delete(self, cdxid):
        """Delete Codex collection"""
        result = db_entries.delete_one({"_id": cdxid})
        if result.deleted_count == 0:
            abort(404, message="Entry not found")
        return result.acknowledged
