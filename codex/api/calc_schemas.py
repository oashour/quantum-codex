"""
Schemas for the Codex API
"""
import os
from datetime import datetime, timezone

from marshmallow import fields, Schema, validate, post_load
from flask_smorest.fields import Upload


from codex.models import CalcCodex
from codex.utils import validate_cdxid
from codex.api.file_schemas import FileCodexSchema


class CalcCodexSchema(Schema):
    """
    Schema for a CalcCodex
    """

    _id = fields.String(required=True, validate=validate_cdxid)
    dbversion = fields.String(required=True)
    code = fields.String(required=True, validate=validate.OneOf(["espresso", "vasp"]))
    file_ids = fields.List(fields.String(validate=validate_cdxid), required=True)
    files = fields.List(fields.Nested(FileCodexSchema))
    created = fields.DateTime(dump_default=datetime.now(timezone.utc), required=True)

    @post_load
    def create_calc(self, data, **kwargs):
        """Deserialize as object"""
        return CalcCodex(**data)


class CalcCodexQueryArgsSchema(Schema):
    """
    Schema for validating query arguments to the calc endpoint.
    """

    cdxid = fields.String(validate=validate_cdxid)
    dbversion = fields.String(required=True)
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]), required=True)


# TODO: Can be merged with previous schema??
class CalcCodexFilesArgsSchema(Schema):
    """
    Schema for validating query arguments to the calc/fromfile endpoint.
    """

    dbversion = fields.String(required=True)
    code = fields.String(validate=validate.OneOf(["espresso", "vasp"]), required=True)


class CalcCodexFilesSchema(Schema):
    """
    Schema for validating query files to the calc endpoint.
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
