"""
Utilities for interacting with the (cdx) database
"""

from flask import current_app
from sqlalchemy.inspection import inspect

from codex.app.api.file_schemas import FileCodexSchema
from codex.app.api.calc_schemas import CalcCodexSchema
from codex.app.models import FileCodexModel, CalcCodexModel, CODEX_MODELS
from codex.app.cdxid import generate_cdxid, get_internal_id_from_cdxid
from codex.core import AbstractFileCodex, CalcCodex
from codex.app.extensions import postgres as db

CODEX_SCHEMAS = {'file': FileCodexSchema, 'calc': CalcCodexSchema}


def check_first_arg_is_list(func):
    """Decorator to make sure first argument is list"""

    def wrapper(*args, **kwargs):
        if not isinstance(args[0], list):
            args = ([args[0]],) + args[1:]
        return func(*args, **kwargs)

    return wrapper


# TODO: this needs a better place
def get_codex(cdxid):
    """
    Gets a codex of any type using a cdxid
    args:
        cdxid: str
            The cdxid of the codex to get
        client: pymongo.MongoClient
            The client to use to access the database
    """
    internalcdxid, codex_type = get_internal_id_from_cdxid(cdxid)
    model = CODEX_MODELS[codex_type].query.get(internalcdxid)
    assign_cdxid(model)
    schema = CODEX_SCHEMAS[codex_type]()
    # TODO: this is pretty atrocious
    return schema.load(schema.dump(model)), codex_type


def insert_codex(codex):
    """
    Inserts a codex of any type into the database
    args:
        codex: codex.models.FileCodex or CalcCodex or ProjectCodex
    """
    if issubclass(type(codex), AbstractFileCodex):
        codex_type = "file"
    elif isinstance(codex, CalcCodex):
        codex_type = "calc"
    else:
        raise TypeError(f"Cannot insert codex of type {type(codex)}")

    with current_app.app_context():
        model = CODEX_MODELS[codex_type](codex)
        db.session.add(model)
        db.session.commit()
        internalcdxid = inspect(model).identity[0]

    return generate_cdxid(internalcdxid, codex_type)

def assign_cdxid(model):
    """Recursively assigns cdxids to codex models based on their primary keys"""
    def assign_single_cdxid(codex_type):
        internalcdxid = inspect(model).identity[0]
        model.cdxid = generate_cdxid(internalcdxid, codex_type)
    #if isinstance(model, ProjCodexModel):
    #    for c in model.calcs:
    #        assign_cdxid(f)
    #    assign_single_cdxid("proj")
    if isinstance(model, CalcCodexModel):
        for f in model.files:
            assign_cdxid(f)
        assign_single_cdxid("calc")
    elif isinstance(model, FileCodexModel):
        assign_single_cdxid("file")
    else:
        raise TypeError(f"Cannot assign CDXIDs to {type(model)}")


