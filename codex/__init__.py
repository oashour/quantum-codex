"""
Package for the Codex flask app
"""


from glob import glob
from importlib import resources
import os
import json
import logging

from flask import Flask

from config import Config


def create_app(config_class=Config):
    """
    Flask app factory
    """
    app = Flask(__name__)
    app.config.from_object(config_class)
    app.jinja_env.lstrip_blocks = True
    app.jinja_env.trim_blocks = True

    # Uploads extension
    from flask_uploads import configure_uploads
    from codex.extensions import inputs

    configure_uploads(app, inputs)

    # MongoDB extension
    from codex.extensions import mongo

    mongo.init_app(app)
    instantiate_database(mongo.cx)

    # Register blueprints
    from codex.upload import bp as upload_bp
    from codex.preview import bp as preview_bp
    from codex.explore import bp as explore_bp

    app.register_blueprint(upload_bp)
    app.register_blueprint(preview_bp)
    app.register_blueprint(explore_bp)

    return app


def instantiate_database(client):
    """
    Instantiate the database with the JSON files

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
        #### Only needed while debugging
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
