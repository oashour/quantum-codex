"""
Utilities for interacting with the (cdx) database
"""

from codex.app.api.file_schemas import FileCodexSchema
from codex.app.api.calc_schemas import CalcCodexSchema
from codex.utils import get_type_from_cdxid

from codex.app.models import AbstractFileCodex, CalcCodex


def check_first_arg_is_list(func):
    """Decorator to make sure first argument is list"""

    def wrapper(*args, **kwargs):
        if not isinstance(args[0], list):
            args = ([args[0]],) + args[1:]
        return func(*args, **kwargs)

    return wrapper


# TODO: this needs a better place
def get_codex(cdxid, client):
    """
    Gets a codex of any type using a cdxid
    args:
        cdxid: str
            The cdxid of the codex to get
        client: pymongo.MongoClient
            The client to use to access the database
    """
    codex_type = get_type_from_cdxid(cdxid)
    print(f"codex_type: {codex_type}")
    if codex_type == "calc":
        return get_calcs(cdxid, client)[0], codex_type
    if codex_type == "file":
        return get_files(cdxid, client)[0], codex_type
    # elif type == "project"
    #    codex = get_project(cdxid, mongo.cx)


def insert_codex(codex, client):
    """
    Inserts a codex of any type into the database
    args:
        codex: codex.models.FileCodex or CalcCodex or ProjectCodex
    """
    if issubclass(type(codex), AbstractFileCodex):
        print("Inserting files")
        insert_files(codex, client)
    elif isinstance(codex, CalcCodex):
        print("Inserting calcs")
        insert_calcs(codex, client)
    else:
        raise TypeError(f"Cannot insert codex of type {type(codex)}")


@check_first_arg_is_list
def insert_files(files, client):
    """
    Inserts a list of FileCodexes into the database with the given PyMongo client
    """
    client["cdx"]["files"].insert_many(FileCodexSchema(many=True).dump(files))


@check_first_arg_is_list
def get_files(cdxids, client, deserialize=True):
    """
    Gets a list of FileCodexes from the database with the given PyMongo client
    """
    if files := list(client["cdx"]["files"].find({"_id": {"$in": cdxids}})):
        return FileCodexSchema(many=True).load(files) if deserialize else files
    else:
        raise KeyError(f"No files found with cdxids {cdxids}")


@check_first_arg_is_list
def insert_calcs(calcs, client):
    """
    Inserts a CalcCodex into the database with the given PyMongo client
    """
    for c in calcs:
        insert_files(c.files, client)
    client["cdx"]["calcs"].insert_many(CalcCodexSchema(exclude=("files",)).dump(calcs, many=True))


@check_first_arg_is_list
def get_calcs(cdxids, client, deserialize=True):
    """
    Gets a CalcCodex into the database with the given PyMongo client
    """
    if not (calcs := list(client["cdx"]["calcs"].find({"_id": {"$in": cdxids}}))):
        raise KeyError(f"No files found with cdxids {cdxids}")
    for c in calcs:
        files = get_files(c["file_ids"], client, deserialize=False)
        if len(files) != len(c["file_ids"]):
            raise KeyError(
                f"Calc {c['_id']} has {len(c['file_ids'])} files, "
                f"but only {len(files)} were found in the database."
            )
        c["files"] = files
    return CalcCodexSchema().load(calcs, many=True) if deserialize else calcs
