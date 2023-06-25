"""
Configuration for the flask app
"""
import os
import secrets

basedir = os.path.abspath(os.path.dirname(__file__))


class Config:
    """
    Base configuration class. Contains default settings and settings applicable to all environments.
    """

    # Default settings
    TESTING = False
    WTF_CSRF_ENABLED = True

    # Settings applicable to all environments
    SECRET_KEY = os.environ.get("SECRET_KEY", default=secrets.token_hex())


class DevelopmentConfig(Config):
    """
    Configuration for development environment
    """

    MONGO_URI = os.environ.get("MONGO_URI", default="mongodb://localhost:27017/")
    UPLOADED_INPUTS_DEST = os.environ.get("UPLOADED_INPUTS_DEST", default="temp_inputs")
    LOG_LEVEL = 'DEBUG'

class TestingConfig(Config):
    """
    Configuration for testing environment
    """

    TESTING = True
    WTF_CSRF_ENABLED = False
    LOG_LEVEL = 'INFO'


class ProductionConfig(Config):
    """
    Configuration for production environment
    """
    LOG_LEVEL = 'INFO'
