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

    # API
    # TODO: update versions and CDN and all that
    API_TITLE = "Codex API"
    API_VERSION = "v0"
    OPENAPI_VERSION = "3.0.2"
    OPENAPI_URL_PREFIX = "/api"
    OPENAPI_SWAGGER_UI_PATH = "/swagger-ui"
    OPENAPI_SWAGGER_UI_URL = "https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/3.24.2/"
    OPENAPI_REDOC_PATH = "/redoc"
    OPENAPI_REDOC_URL = (
        "https://cdn.jsdelivr.net/npm/redoc@2.0.0-alpha.17/bundles/redoc.standalone.js"
    )
    OPENAPI_RAPIDOC_PATH = "/rapidoc"
    OPENAPI_RAPIDOC_URL = "https://cdn.jsdelivr.net/npm/rapidoc@9.0.0/dist/rapidoc-min.js"


class DevelopmentConfig(Config):
    """
    Configuration for development environment
    """

    MONGO_URI = os.environ.get("MONGO_URI", default="mongodb://localhost:27017/")
    UPLOADED_INPUTS_DEST = os.environ.get("UPLOADED_INPUTS_DEST", default="temp_inputs")
    LOG_LEVEL = "DEBUG"


class TestingConfig(Config):
    """
    Configuration for testing environment
    """

    TESTING = True
    WTF_CSRF_ENABLED = False
    LOG_LEVEL = "INFO"


class ProductionConfig(Config):
    """
    Configuration for production environment
    """

    LOG_LEVEL = "INFO"
