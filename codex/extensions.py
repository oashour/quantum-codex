from flask_uploads import UploadSet
from flask_pymongo import PyMongo
from flask_smorest import Api

inputs = UploadSet("inputs", ["pwi", "in", "vasp"])
mongo = PyMongo()
api = Api()
