"""
Schemas for the Codex API
"""
import os
from datetime import datetime, timezone

from marshmallow import fields, Schema, validate, post_load

from codex.core import CodexTag, FILE_CODEX_MAP

from codex.app.cdxid import validate_cdxid


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

    cdxid = fields.String(required=True, validate=validate_cdxid)
    time_created = fields.DateTime()

    tags = fields.Dict(
        keys=fields.Str(), values=fields.List(fields.Nested(CodexTagSchema)), required=True
    )
    cards = fields.String(required=True)
    name = fields.String(required=True)
    raw_file = fields.String(required=True)
    filetype = fields.String(required=True)
    code = fields.String(required=True)
    dbversion = fields.String(required=True)

    comment_token = fields.String(required=False)
    section_start_token = fields.String(required=False)
    section_end_token = fields.String(required=False)
    indent = fields.Integer(required=False)

    # TODO: this shouldn't be necessary
    @post_load
    def create_codex(self, data, **kwargs):
        """Deserialize as object"""
        data["filename"] = data["name"]
        Codex = FILE_CODEX_MAP[data["code"]]
        return Codex(**data)


class FileCodexQueryArgsSchema(Schema):
    """
    Schema for validating query arguments to the file endpoint.
    """

    cdxid = fields.UUID(data_key="cdxid", attribute="cdxid")
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]))
    filetype = fields.String()  # TODO: validator?
