"""
This module contains utility functions for the API.
"""
from codex.api.file_schemas import FileCodexSchema
from codex.api.calc_schemas import CalcCodexSchema


# TODO: this needs a better place
def insert_calc(calc, client):
    """
    Inserts a CodexCollection into the database with the given PyMongo client
    """
    client["cdx"]["files"].insert_many(FileCodexSchema().dump(calc.files, many=True))
    client["cdx"]["calcs"].insert_one(
        CalcCodexSchema(exclude=("files",)).dump(calc)
    )


# TODO: this needs a better place
def get_calc(cdxid, client):
    """
    Inserts a CodexCollection into the database with the given PyMongo client
    """
    calc = client["cdx"]["calcs"].find_one({"_id": cdxid})
    if not (calc := client["cdx"]["calcs"].find_one({"_id": cdxid})):
        raise KeyError(f"Collection {cdxid} not found")

    files = list(client["cdx"]["files"].find({"_id": {"$in": calc["file_ids"]}}))
    if len(files) != len(calc["file_ids"]):
        raise KeyError(
            f"Calc {cdxid} has {len(calc['file_ids'])} files, "
            f"but only {len(files)} were found in the database."
        )

    calc["files"] = files
    return CalcCodexSchema().load(calc)
