"""
Schemas for the Codex API
"""
import os
from datetime import datetime, timezone

from marshmallow import fields, Schema, validate, post_load


class CodexEntrySchema(Schema):
    """Schema for Codex objects"""

    class CodexTagSchema(Schema):
        """Schema for a Codex's tags"""

        name = fields.String(required=True)
        id = fields.String(required=True)
        value = fields.String(required=True)
        comment = fields.String(required=True)
        tag_pad = fields.Integer(required=True)
        value_pad = fields.Integer(required=True)
        comment_pad = fields.Integer(required=True)

    _id = fields.UUID(required=True)
    created = fields.DateTime(required=True,dump_default=datetime.now(timezone.utc))

    tags = fields.Dict(
        keys=fields.Str(), values=fields.List(fields.Nested(CodexTagSchema)), required=True
    )
    cards = fields.String(required=True)
    filename = fields.String(required=True)
    filetype = fields.String(required=True)
    code = fields.String(required=True)
    dbversion = fields.String(required=True)
    code_pretty = fields.String(required=True)
    comment_token = fields.String(required=True)
    section_start_token = fields.String(required=True)
    section_end_token = fields.String(required=True)
    pad = fields.String(required=True)
    indent = fields.Integer(required=True)

    # TODO: this shouldn't be necessary
    @post_load
    def hex_uuid(self, data, **kwargs):
        if "_id" in data:
            data["_id"] = data["_id"].hex
        return data


class CodexEntryQueryArgsSchema(Schema):
    """
    Schema for validating query arguments to the collections endpoint.
    """

    uuid = fields.UUID(data_key="_id", attribute="_id")
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]))
    filetype = fields.String()  # TODO: validator?

    # TODO: this shouldn't be necessary
    @post_load
    def hex_uuid(self, data, **kwargs):
        if "_id" in data:
            data["_id"] = data["_id"].hex
        return data
