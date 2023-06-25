"""
Configuration for the flask app
"""
import os

basedir = os.path.abspath(os.path.dirname(__file__))


class Config:
    """
    Configuration class for the flask app
    """

    SECRET_KEY = os.environ.get("SECRET_KEY") or os.urandom(24)
    MONGO_URI = os.environ.get("MONGO_URI") or "mongodb://localhost:27017/myDatabase"
    UPLOADED_INPUTS_DEST = os.environ.get("UPLOADED_INPUTS_DEST") or "temp_inputs"
