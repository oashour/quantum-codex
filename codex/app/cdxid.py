import math
import os

from ksuid import Ksuid
from ff3 import FF3Cipher
from baseconv import base62

# TODO: move this
CDX_KEY = os.environ.get(
    "CDX_KEY", default="9f758ef5d89c2a6b721c69a7d8767dba77a411039d1ba48516e02a7a9ddc641a"
)  # os.urandom(32).hex()
CDX_TWEAK = os.environ.get("CDX_TWEAK", default="bfe8b69177da9d")  # os.urandom(7).hex()

CODEX_PREFIXES = {"file": "F", "calc": "C", "proj": "P"}
CODEX_TYPES = {v: k for k, v in CODEX_PREFIXES.items()}

BASE62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
CDX_CIPHER = FF3Cipher.withCustomAlphabet(CDX_KEY, CDX_TWEAK, BASE62)


class ShortKsuid(Ksuid):
    """
    A shorter version of the ksuid, with a payload length of 8 bytes instead of 16
    """

    PAYLOAD_LENGTH_IN_BYTES = 8
    BYTES_LENGTH = Ksuid.TIMESTAMP_LENGTH_IN_BYTES + PAYLOAD_LENGTH_IN_BYTES
    BASE62_LENGTH = math.ceil(BYTES_LENGTH * 4 / 3)

    def __str__(self) -> str:
        """Creates a base62 string representation"""
        return base62.encode(int.from_bytes(bytes(self), "big")).zfill(self.BASE62_LENGTH)


def generate_cdxid(id: str, codex_type: str):
    """
    Generates a cdxid from an id (internal primary key) and codex type
    """
    prefix = CODEX_PREFIXES[codex_type]
    prefixed_pk = f"{prefix}{id}"
    return f"cdx-{_encrypt(prefixed_pk)}"


def get_id_from_cdxid(cdxid):
    """
    Gets the primary key and codex type from a cdxid
    params:
        cdxid: a cdxid (in the form cdx-<encrypted_pk_with_prefix>)
    """
    prefixed_pk = _decrypt(cdxid[4:])
    return prefixed_pk[1:], CODEX_TYPES[prefixed_pk[:1]]


def _encrypt(plaintext: str):
    """Encrypts plaintext with the CDX_CIPHER"""
    return CDX_CIPHER.encrypt(plaintext)


def _decrypt(ciphertext: str):
    """Decrypts ciphertext with the CDX_CIPHER"""
    return CDX_CIPHER.decrypt(ciphertext)
