from flask_uploads import UploadSet
from flask_pymongo import PyMongo

inputs = UploadSet("inputs", ["pwi", "in", "vasp"])
mongo = PyMongo()
