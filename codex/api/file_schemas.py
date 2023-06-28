"""
Schemas for the Codex API
"""
import os
from datetime import datetime, timezone

from marshmallow import fields, Schema, validate, post_load

from codex.models import CodexTag, FILE_CODEX_MAP

from codex.utils import validate_cdxid


class CodexTagSchema(Schema):
    """Schema for a Codex's tags"""

    name = fields.String(required=True)
    value = fields.String(required=True)
    comment = fields.String(required=True)
    tag_pad = fields.String(required=True)
    value_pad = fields.String(required=True)
    comment_pad = fields.String(required=True)
    id = fields.String(required=True)
    href = fields.String(required=True)
    indent = fields.String(required=True)

    @post_load
    def create_tag(self, data, **kwargs):
        """Deserialize as object"""
        return CodexTag(**data)


class FileCodexSchema(Schema):
    """Schema for Codex objects"""

    _id = fields.String(required=True, validate=validate_cdxid)
    created = fields.DateTime(required=True, dump_default=datetime.now(timezone.utc))

    tags = fields.Dict(
        keys=fields.Str(), values=fields.List(fields.Nested(CodexTagSchema)), required=True
    )
    cards = fields.String(required=True)
    filename = fields.String(required=True)
    raw_file = fields.String(required=True)
    filetype = fields.String(required=True)
    code = fields.String(required=True)
    dbversion = fields.String(required=True)
    comment_token = fields.String(required=True)
    section_start_token = fields.String(required=True)
    section_end_token = fields.String(required=True)
    indent = fields.Integer(required=True)

    # TODO: this shouldn't be necessary
    @post_load
    def create_codex(self, data, **kwargs):
        """Deserialize as object"""
        Codex = FILE_CODEX_MAP[data["code"]]
        return Codex(**data)


class FileCodexQueryArgsSchema(Schema):
    """
    Schema for validating query arguments to the collections endpoint.
    """

    cdxid = fields.UUID(data_key="_id", attribute="_id")
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]))
    filetype = fields.String()  # TODO: validator?
