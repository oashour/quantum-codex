"""
Schemas for the Codex API
"""
import os
from datetime import datetime, timezone

from marshmallow import fields, Schema, validate, post_load
from flask_smorest.fields import Upload


from codex.models import CodexCollection
from codex.utils import validate_cdxid
from codex.api.entry_schemas import CodexEntrySchema


class CodexCollectionSchema(Schema):
    """
    Schema for a Codex collection
    """

    _id = fields.String(required=True, validate=validate_cdxid)
    dbversion = fields.String(required=True)
    code = fields.String(required=True, validate=validate.OneOf(["espresso", "vasp"]))
    entry_ids = fields.List(fields.UUID(), required=True)
    entries = fields.List(fields.Nested(CodexEntrySchema))
    created = fields.DateTime(dump_default=datetime.now(timezone.utc), required=True)

    @post_load
    def create_collection(self, data, **kwargs):
        """Deserialize as object"""
        return CodexCollection(**data)


class CodexCollectionQueryArgsSchema(Schema):
    """
    Schema for validating query arguments to the collections endpoint.
    """

    cdxid = fields.String(validate=validate_cdxid)
    dbversion = fields.String(required=True)
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]), required=True)


class CodexCollectionFilesArgsSchema(Schema):
    """
    Schema for validating query arguments to the collections endpoint.
    """

    dbversion = fields.String(required=True)
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]), required=True)


class CodexCollectionFilesSchema(Schema):
    """
    Schema for validating query files to the collections endpoint.
    """

    input_file = fields.List(Upload(), required=True)

    # dbversion = fields.String(required=True)
    # code = fields.String(required=True)
    @post_load
    def clean_filenames(self, data, **kwargs):
        """Cleans up filenames to exclude path"""
        for f in data["input_file"]:
            f.filename = os.path.basename(f.filename)
        return data
