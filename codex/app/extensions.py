"""
Flask extensions used by Codex
"""
import re
from werkzeug.datastructures import FileStorage
from flask_uploads import UploadSet
from flask_uploads.extensions import extension
from flask_pymongo import PyMongo
from flask_smorest import Api


class CodexUploadSet(UploadSet):
    """
    Custom UploadSet for Codex, checks the extension of the file or the start of the file name
    """

    def file_allowed(self, storage: FileStorage, basename: str) -> bool:
        return re.match(
            r"^(INCAR|POSCAR|KPOINTS|POTCAR)", storage.filename
        ) or self.extension_allowed(extension(basename))


inputs = CodexUploadSet("inputs", ["pwi", "in", "vasp"])
mongo = PyMongo()
api = Api()
