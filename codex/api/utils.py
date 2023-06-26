"""
This module contains utility functions for the API.
"""
from codex.api.entry_schemas import CodexEntrySchema
from codex.api.collection_schemas import CodexCollectionSchema


# TODO: this needs a better place
def insert_collection(collection, client):
    """
    Inserts a CodexCollection into the database with the given PyMongo client
    """
    client["codex"]["entries"].insert_many(CodexEntrySchema().dump(collection.entries, many=True))
    client["codex"]["collections"].insert_one(
        CodexCollectionSchema(exclude=("entries",)).dump(collection)
    )


# TODO: this needs a better place
def get_collection(cdxid, client):
    """
    Inserts a CodexCollection into the database with the given PyMongo client
    """
    collection = client["codex"]["collections"].find_one({"_id": cdxid})
    if not (collection := client["codex"]["collections"].find_one({"_id": cdxid})):
        raise KeyError(f"Collection {cdxid} not found")

    entries = list(client["codex"]["entries"].find({"_id": {"$in": collection["entry_ids"]}}))
    print(f"Got entries {entries}")
    print(f"There are {len(entries)} entries in the database")
    if len(entries) != len(collection["entry_ids"]):
        raise KeyError(
            f"Collection {cdxid} has {len(collection['entry_ids'])} entries, "
            f"but only {len(entries)} were found in the database."
        )

    collection["entries"] = entries
    return CodexCollectionSchema().load(collection)
