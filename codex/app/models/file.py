"""
Model for the FileCodex
"""
from sqlalchemy.sql import func
from sqlalchemy.dialects.postgresql import JSONB

from codex.app.cdxid import ShortKsuid
from codex.app.extensions import postgres as db


class FileCodexModel(db.Model):
    """
    Model for the files table, see codex.core.FileCodex
    """

    __tablename__ = "file"

    file_id = db.Column(
        db.String(ShortKsuid.BASE62_LENGTH), primary_key=True, default=lambda: str(ShortKsuid())
    )
    time_created = db.Column(db.DateTime(timezone=True), server_default=func.now())
    time_updated = db.Column(db.DateTime(timezone=True), onupdate=func.now())

    name = db.Column(db.String(), nullable=False)
    raw_file = db.Column(db.String(), nullable=False)
    code = db.Column(db.String(), nullable=False)
    dbversion = db.Column(db.String(), nullable=False)
    filetype = db.Column(db.String(), nullable=False)
    cards = db.Column(db.String(), nullable=False)

    # tags = db.relationship("TagModel", backref="files", lazy=True)
    tags = db.Column(JSONB, nullable=False)
    calc_id = db.Column(
        db.String(ShortKsuid.BASE62_LENGTH), db.ForeignKey("calc.calc_id"), nullable=True
    )

    def __init__(self, file_codex):
        self.name = file_codex.name
        self.raw_file = file_codex.raw_file
        self.code = file_codex.code
        self.dbversion = file_codex.dbversion
        self.filetype = file_codex.filetype
        self.tags = {section: [vars(t) for t in tags] for section, tags in file_codex.tags.items()}
        self.cards = file_codex.cards

    def __repr__(self):
        return f"<file_id {self.file_id}>"
