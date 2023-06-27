"""
Package for the Codex flask app
"""


from glob import glob
from importlib import resources
import os
import json
import logging
import warnings
from logging.handlers import RotatingFileHandler

from flask import Flask, render_template, abort
from flask.logging import default_handler

from config import Config


def create_app():
    """
    Flask app factory
    """
    app = Flask(__name__)
    CONFIG_TYPE = os.getenv("CONFIG_TYPE", default="config.DevelopmentConfig")
    app.config.from_object(CONFIG_TYPE)
    app.jinja_env.lstrip_blocks = True
    app.jinja_env.trim_blocks = True
    # Inelegant solution for dealing with this warning that doesn't affect us
    # See https://github.com/marshmallow-code/apispec/issues/444
    warnings.filterwarnings(
        "ignore", message="Multiple schemas resolved to the name CodexCollection."
    )

    configure_extensions(app)
    configure_blueprints(app)
    configure_logging(app)
    configure_error_handlers(app)

    return app


def configure_extensions(app):
    """
    Helper function to configure extensions
    """
    # Uploads extension
    from flask_uploads import configure_uploads
    from codex.extensions import inputs

    configure_uploads(app, inputs)

    # MongoDB extension
    from codex.extensions import mongo

    mongo.init_app(app)
    _instantiate_database(mongo.cx, app)

    # API extension
    from codex.extensions import api
    from codex.api import entries_bp, collections_bp

    api.init_app(app)
    api.register_blueprint(entries_bp)
    api.register_blueprint(collections_bp)


def configure_logging(app):
    """
    Helper function to configure logging
    """
    # Deactivate the default flask logger to avoid duplication
    app.logger.removeHandler(default_handler)

    # RotatingFileHandler: create new log when file reaches 16KB, keep 20 old logs
    file_handler = RotatingFileHandler("logs/codex.log", maxBytes=16384, backupCount=20)
    file_handler.setLevel(app.config["LOG_LEVEL"])
    file_formatter = logging.Formatter(
        "%(asctime)s | %(levelname)s: %(message)s [in %(pathname)s: %(lineno)d]"
    )
    file_handler.setFormatter(file_formatter)

    app.logger.addHandler(file_handler)


def configure_blueprints(app):
    """
    Helper function to configure blueprints
    """
    from codex.main import bp as main_bp

    app.register_blueprint(main_bp)


def configure_error_handlers(app):
    """
    Helper function to register error handlers
    """

    # 400 - Bad Request
    @app.errorhandler(400)
    def bad_request(e):
        return render_template("error.html.j2", error_title="400 - Bad Request"), 400

    # 403 - Forbidden
    @app.errorhandler(403)
    def forbidden(e):
        return render_template("error.html.j2", error_title="403 - Forbidden"), 403

    # 404 - Page Not Found
    @app.errorhandler(404)
    def page_not_found(e):
        return render_template("error.html.j2", error_title="404 - Page Not Found"), 404

    # 405 - Method Not Allowed
    @app.errorhandler(405)
    def method_not_allowed(e):
        return render_template("error.html.j2", error_title="405 - Method Not Allowed"), 405

    # 500 - Internal Server Error
    @app.errorhandler(500)
    def server_error(e):
        return render_template("error.html.j2", error_title="500 - Internal Server Error"), 500


def _instantiate_database(client, app):
    """
    Helper function to instantiate the database from the JSON files

    TODO: should make this smarter and create the JSON from scratch,
    figure out most recent QE versions from GitLab, etc.
    """

    def insert_json(json_file):
        """
        Insert the JSON into MongoDB
        Replaces dots in the filename with carets to avoid issues with MongoDB
        """
        with open(json_file, "r") as f:
            database_json = json.load(f)

        database_name = os.path.splitext(os.path.basename(json_file))[0].replace(".", "^")
        if database_name not in client.list_database_names():
            logging.info(f"Building database {database_name}.")
            db = client[database_name]
            for file_type, tags in database_json.items():
                db[file_type].insert_many(tags)
        logging.warning(f"Database {database_name} already exists. Skipping.")

    base_db_dir = resources.files("codex.database")
    json_files = glob(os.path.join(base_db_dir, "json", "*.json"))
    for file in json_files:
        insert_json(file)
