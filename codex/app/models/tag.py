"""
Model for the CodexTag
"""
from sqlalchemy.sql import func
from ksuid import KsuidMs

from codex.app.cdxid import ShortKsuid
from codex.app.extensions import postgres as db


class TagModel(db.Model):
    """
    Model for the tags table, see codex.core.Tag
    """

    __tablename__ = "tags"

    id = db.Column(db.String(KsuidMs.BASE62_LENGTH), primary_key=True, default=KsuidMs)
    time_created = db.Column(db.DateTime(timezone=True), server_default=func.now())
    time_updated = db.Column(db.DateTime(timezone=True), onupdate=func.now())

    name = db.Column(db.String(), nullable=False)
    value = db.Column(db.String(), nullable=False)
    comment = db.Column(db.String(), nullable=False)
    raw_name = db.Column(db.String(), nullable=False)
    # TODO: change these to integers
    indent = db.Column(db.String(), nullable=False)
    tag_pad = db.Column(db.String(), nullable=False)
    value_pad = db.Column(db.String(), nullable=False)
    comment_pad = db.Column(db.String(), nullable=False)

    href = db.Column(db.String(), nullable=False)  # TODO: won't need this soon
    file_id = db.Column(
        db.String(ShortKsuid.BASE62_LENGTH), db.ForeignKey("files.id"), nullable=False
    )

    def __init__(self, input):
        for key, value in input.items():
            setattr(self, key, value)

    def __repr__(self):
        return f"<id {self.id}>"
