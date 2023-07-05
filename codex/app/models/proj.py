"""
Model for the ProjCodex
"""
from codex.app.models import CalcCodexModel
from codex.app.extensions import postgres as db


class ProjCodexModel(db.Model):
    """
    Model for the calcs table, see codex.core.CalcCodex
    """

    __tablename__ = "proj"

    proj_id = db.Column(db.Integer, primary_key=True)
    # TODO: add length contraint
    cdxid = db.Column(db.String(), unique=True, nullable=False)
    name = db.Column(db.String(), nullable=False)
    code = db.Column(db.String(), nullable=False)
    dbversion = db.Column(db.String(), nullable=False)
    created = db.Column(db.DateTime, nullable=False)

    calcs = db.relationship("CalcCodexModel", backref="calc", lazy=True)

    def __init__(self, file_codex):
        self.name = file_codex.name
        self.raw_file = file_codex.raw_file
        self.code = file_codex.code
        self.dbversion = file_codex.dbversion
        self.filetype = file_codex.filetype
        self.cards = file_codex.cards

    def __repr__(self):
        return f"<proj_id {self.proj_id}>"
