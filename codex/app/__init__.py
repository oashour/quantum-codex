"""
Package for the Codex flask app
"""


from glob import glob
from importlib import resources
import os
import json
import logging
import warnings
from logging.handlers import TimedRotatingFileHandler
from logging.config import dictConfig

from flask import Flask, render_template, abort, has_request_context, request
from flask.logging import default_handler

from config import Config

# TODO: this is sort of ugly, maybe an enum? Also should be moved elsewhere
STD_CODE_MAP = {"VASP": "vasp", "Quantum ESPRESSO": "espresso"}
PRETTY_CODE_MAP = {v: k for k, v in STD_CODE_MAP.items()}


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
    warnings.filterwarnings("ignore", message="Multiple schemas resolved to the name CalcCodex.")

    configure_logging(app)
    configure_extensions(app)
    configure_blueprints(app)
    configure_error_handlers(app)

    return app


def configure_extensions(app):
    """
    Helper function to configure extensions
    """
    # Uploads extension
    from flask_uploads import configure_uploads
    from codex.app.extensions import inputs

    configure_uploads(app, inputs)

    # MongoDB extension
    from codex.app.extensions import mongo

    mongo.init_app(app)
    _instantiate_mongo(mongo.cx, app)

    # PostgreSQL extension
    from codex.app.extensions import postgres

    postgres.init_app(app)
    _instantiate_postgres(postgres, app)

    # API extension
    from codex.app.extensions import api
    from codex.app.api import files_bp, calcs_bp

    api.init_app(app)
    api.register_blueprint(files_bp)
    api.register_blueprint(calcs_bp)


def configure_logging(app):
    """
    Helper function to configure logging
    We intentionally keep the default_handler for additional logging to stdout
    """

    class RequestFormatter(logging.Formatter):
        def format(self, record):
            if has_request_context():
                record.url = request.url
                record.remote_addr = request.remote_addr
            else:
                record.url = None
                record.remote_addr = None

            return super().format(record)

    formatter = RequestFormatter(
        "[%(asctime)s] %(levelname)-8s %(remote_addr)s requested %(url)s\n"
        "in [%(filename)s:%(module)s:%(funcName)s:%(lineno)d]: %(message)s"
    )
    default_handler.setFormatter(formatter)
    default_handler.setLevel(app.config["LOG_LEVEL"])

    file_handler = TimedRotatingFileHandler("logs/logfile.log", when="midnight", backupCount=90)
    file_handler.setLevel(app.config["LOG_LEVEL"])
    file_handler.setFormatter(formatter)

    app.logger.addHandler(file_handler)


def configure_blueprints(app):
    """
    Helper function to configure blueprints
    """
    from codex.app.main import bp as main_bp

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


def _instantiate_postgres(db, app):
    # sourcery skip: extract-duplicate-method, inline-immediately-returned-variable
    """
    Helper function to instantiate the postgres database
    """
    from codex.app.models import CalcCodexModel, FileCodexModel, TagModel

    with app.app_context():
        db.drop_all()
        db.create_all()

def _instantiate_mongo(client, app):
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
        if database_name in client.list_database_names():
            if app.debug:
                app.logger.warning(
                    f"Database {database_name} already exists. Skipping (Debug = {app.debug})."
                )
            else:
                app.logger.warning(f"Database {database_name} already exists. Dropping.")
                client.drop_database(database_name)
        else:
            db = client[database_name]
            app.logger.info(f"Building database {database_name}.")
            for file_type, tags in database_json.items():
                db[file_type].insert_many(tags)

    base_db_dir = resources.files("codex.database")
    json_files = glob(os.path.join(base_db_dir, "json", "*.json"))
    for file in json_files:
        insert_json(file)
