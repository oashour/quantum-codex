"""
Model for the CalcCodex
"""
from sqlalchemy.sql import func

from codex.app.models import FileCodexModel
from codex.app.cdxid import ShortKsuid
from codex.app.extensions import postgres as db


class CalcCodexModel(db.Model):
    """
    Model for the calcs table, see codex.core.CalcCodex
    """

    __tablename__ = "calcs"

    id = db.Column(
        db.String(ShortKsuid.BASE62_LENGTH), primary_key=True, default=lambda: str(ShortKsuid())
    )
    time_created = db.Column(db.DateTime(timezone=True), server_default=func.now())
    time_updated = db.Column(db.DateTime(timezone=True), onupdate=func.now())

    # TODO: add length contraint
    name = db.Column(db.String(), nullable=False)
    readme = db.Column(db.String(), nullable=False)
    code = db.Column(db.String(), nullable=False)
    dbversion = db.Column(db.String(), nullable=False)

    files = db.relationship("FileCodexModel", backref="files", lazy=True)
    # proj_id = db.Column(db.Integer, db.ForeignKey("projs.id"), nullable=True)

    def __init__(self, calc_codex):
        self.name = calc_codex.name
        self.readme = calc_codex.readme
        self.code = calc_codex.code
        self.dbversion = calc_codex.dbversion
        self.files = [FileCodexModel(f) for f in calc_codex.files]

    def __repr__(self):
        return f"<id {self.id}>"
