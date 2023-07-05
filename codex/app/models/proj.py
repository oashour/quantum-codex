"""
Model for the ProjCodex
"""
from .app import db
from . import CUID_GENERATOR


class ProjCodexModel(db.Model):
    """
    Model for the calcs table, see codex.core.CalcCodex
    """

    __tablename__ = "calcs"

    id = db.Column(db.Integer, primary_key=True)
    # TODO: add length contraint
    cdxid = db.Column(db.String(), unique=True, nullable=False)
    name = db.Column(db.String(), nullable=False)
    code = db.Column(db.String(), nullable=False)
    dbversion = db.Column(db.String(), nullable=False)
    created = db.Column(db.DateTime, nullable=False)

    files = db.relationship("FileCodexModel", backref="files", lazy=True)
    proj_id = db.Column(db.Integer, db.ForeignKey("projs.id"), nullable=True)

    def __init__(self, file_codex):
        self.name = file_codex.name
        self.raw_file = file_codex.raw_file
        self.code = file_codex.code
        self.dbversion = file_codex.dbversion
        self.filetype = file_codex.filetype
        self.cards = file_codex.cards

    def __repr__(self):
        return f"<id {self.id}>"
