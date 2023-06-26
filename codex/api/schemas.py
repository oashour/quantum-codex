"""
Schemas for the Codex API
"""
import json

from marshmallow import fields, Schema, validate, post_load, post_dump, ValidationError


from codex.utils import validate_cdxid


class CodexEntrySchema(Schema):
    """
    Schema for Codex objects
    """

    class CodexTagSchema(Schema):
        """
        Schema for a Codex's tags
        """

        class Meta:
            additional = (
                "name",
                "value",
                "comment",
                "tag_pad",
                "value_pad",
                "comment_pad",
                "id",
                "href",
            )

    _id = fields.UUID(required=True)

    tags = fields.Dict(
        keys=fields.Str(), values=fields.List(fields.Nested(CodexTagSchema)), required=True
    )
    cards = fields.String(required=True)

    @post_load
    def hex_uuid(self, data, **kwargs):
        data["_id"] = data["_id"].hex
        return data

    class Meta:
        # We could add validation on code and code_pretty but it's not really necessary
        # because they're already dealt with in the Codex model
        additional = (
            "filename",
            "code",
            "filetype",
            "comment_token",
            "section_start_token",
            "section_end_token",
            "code_pretty",
            "cards",
            "pad",
        )


class CodexEntryQueryArgsSchema(Schema):
    """
    Schema for validating query arguments to the collections endpoint.
    """

    uuid = fields.UUID(data_key="_id", attribute="_id")
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]))

    @post_load
    def hex_uuid(self, data, **kwargs):
        if "_id" in data:
            data["_id"] = data["_id"].hex
        return data

    class Meta:
        additional = ("filetype",)


class CodexCollectionSchema(Schema):
    """
    Schema for a Codex collection
    """

    cdxid = fields.String(required=True, validate=validate_cdxid, data_key="_id")
    dbversion = fields.String(required=True)
    code = fields.String(required=True, validate=validate.OneOf(["espresso", "vasp"]))
    entries = fields.List(fields.UUID(), required=True)


class CodexCollectionQueryArgsSchema(Schema):
    """
    Schema for validating query arguments to the collections endpoint.
    """

    cdxid = fields.String(validate=validate_cdxid)
    dbversion = fields.String()
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]))

class CodexCollectionQueryFilesSchema(Schema):
    """
    Schema for validating query files to the collections endpoint.
    """

    # TODO: need form/file schema
    cdxid = fields.String(validate=validate_cdxid)
    dbversion = fields.String()
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]))