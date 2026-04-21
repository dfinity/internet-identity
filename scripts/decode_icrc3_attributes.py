#!/usr/bin/env python3
"""
Decode an ICRC-3 attribute blob returned by Internet Identity's
`get_icrc3_attributes` (data) + `prepare_icrc3_attributes` (signature) pair.

The input JSON looks like:
    {"data": "<base64 Candid blob>", "signature": "<base64 CBOR blob>"}

`data`      is a Candid-encoded ICRC-3 Value (typically a Map of attribute
            names -> values).
`signature` is a CBOR-encoded IC signature envelope (certificate + tree +
            delegation) as produced by the management canister.

Usage:
    python3 decode_icrc3_attributes.py <input.json>
    python3 decode_icrc3_attributes.py -              # read from stdin
    echo '{"data": "...", "signature": "..."}' | python3 decode_icrc3_attributes.py -

Requires: cbor2  (pip install cbor2)
"""

from __future__ import annotations

import base64
import json
import sys
from dataclasses import dataclass
from typing import Any

import cbor2


# ---------------------------------------------------------------------------
# Base64 helper — pads a possibly-unpadded blob before decoding.
# ---------------------------------------------------------------------------


def b64decode(s: str) -> bytes:
    return base64.b64decode(s + "=" * (-len(s) % 4))


# ---------------------------------------------------------------------------
# Minimal Candid decoder — sufficient for ICRC-3 Value (recursive variant of
# Nat / Int / Blob / Text / Array / Map).
# ---------------------------------------------------------------------------


# Candid primitive opcodes (negative sleb128).
PRIM = {
    -1: "null",
    -2: "bool",
    -3: "nat",
    -4: "int",
    -5: "nat8",
    -6: "nat16",
    -7: "nat32",
    -8: "nat64",
    -9: "int8",
    -10: "int16",
    -11: "int32",
    -12: "int64",
    -13: "float32",
    -14: "float64",
    -15: "text",
    -16: "reserved",
    -17: "empty",
    -24: "principal",
}

# Composite-type opcodes.
T_OPT = -18
T_VEC = -19
T_RECORD = -20
T_VARIANT = -21
T_FUNC = -22
T_SERVICE = -23


class Reader:
    def __init__(self, buf: bytes):
        self.buf = buf
        self.pos = 0

    def eof(self) -> bool:
        return self.pos >= len(self.buf)

    def read(self, n: int) -> bytes:
        out = self.buf[self.pos : self.pos + n]
        if len(out) != n:
            raise ValueError(f"unexpected EOF (need {n}, have {len(out)})")
        self.pos += n
        return out

    def uleb(self) -> int:
        result, shift = 0, 0
        while True:
            b = self.read(1)[0]
            result |= (b & 0x7F) << shift
            if b & 0x80 == 0:
                return result
            shift += 7

    def sleb(self) -> int:
        result, shift = 0, 0
        while True:
            b = self.read(1)[0]
            result |= (b & 0x7F) << shift
            shift += 7
            if b & 0x80 == 0:
                if b & 0x40:
                    result |= -(1 << shift)
                return result


@dataclass
class CType:
    """A decoded Candid type (from the type table)."""

    kind: str  # "prim" | "opt" | "vec" | "record" | "variant" | "ref"
    value: Any = None  # prim name, inner type-ref, or list of (hash, type-ref)


def _parse_type(reader: Reader) -> CType:
    op = reader.sleb()
    if op < 0 and op in PRIM:
        return CType("prim", PRIM[op])
    if op == T_OPT:
        return CType("opt", reader.sleb())
    if op == T_VEC:
        return CType("vec", reader.sleb())
    if op == T_RECORD:
        n = reader.uleb()
        fields = [(reader.uleb(), reader.sleb()) for _ in range(n)]
        return CType("record", fields)
    if op == T_VARIANT:
        n = reader.uleb()
        fields = [(reader.uleb(), reader.sleb()) for _ in range(n)]
        return CType("variant", fields)
    raise ValueError(f"unsupported Candid type opcode {op}")


def _resolve(ref: int, table: list[CType]) -> CType:
    """Resolve a type reference: negative => primitive, non-negative => table index."""
    if ref < 0:
        if ref in PRIM:
            return CType("prim", PRIM[ref])
        raise ValueError(f"unknown primitive type {ref}")
    return table[ref]


# Well-known ICRC-3 Value variant tags — hashed field names per Candid spec.
# hash = sum(b * 223^i for i, b in enumerate(name.bytes)) mod 2^32
def candid_hash(name: str) -> int:
    h = 0
    for b in name.encode("utf-8"):
        h = (h * 223 + b) & 0xFFFFFFFF
    return h


_ICRC3_TAGS = {candid_hash(n): n for n in ("Nat", "Int", "Blob", "Text", "Array", "Map")}


def _decode_value(reader: Reader, typ: CType, table: list[CType]) -> Any:
    """Decode a single Candid value given its type."""
    if typ.kind == "prim":
        return _decode_prim(reader, typ.value)
    if typ.kind == "vec":
        n = reader.uleb()
        inner = _resolve(typ.value, table)
        return [_decode_value(reader, inner, table) for _ in range(n)]
    if typ.kind == "opt":
        flag = reader.read(1)[0]
        if flag == 0:
            return None
        inner = _resolve(typ.value, table)
        return _decode_value(reader, inner, table)
    if typ.kind == "record":
        out = {}
        for hash_id, ref in typ.value:
            inner = _resolve(ref, table)
            out[hash_id] = _decode_value(reader, inner, table)
        # Rewrite known-hash keys as strings where we can (best-effort).
        return _rewrite_known_hashes(out)
    if typ.kind == "variant":
        idx = reader.uleb()
        hash_id, ref = typ.value[idx]
        tag = _ICRC3_TAGS.get(hash_id, f"hash_{hash_id}")
        inner = _resolve(ref, table)
        return {tag: _decode_value(reader, inner, table)}
    raise ValueError(f"cannot decode kind {typ.kind}")


def _decode_prim(reader: Reader, name: str) -> Any:
    if name == "nat":
        return reader.uleb()
    if name == "int":
        return reader.sleb()
    if name == "text":
        n = reader.uleb()
        return reader.read(n).decode("utf-8")
    if name == "bool":
        return bool(reader.read(1)[0])
    if name == "null":
        return None
    if name == "reserved":
        return None
    if name.startswith("nat") and name[3:].isdigit():
        return int.from_bytes(reader.read(int(name[3:]) // 8), "little", signed=False)
    if name.startswith("int") and name[3:].isdigit():
        return int.from_bytes(reader.read(int(name[3:]) // 8), "little", signed=True)
    if name == "float64":
        import struct

        return struct.unpack("<d", reader.read(8))[0]
    raise ValueError(f"primitive {name} not implemented")


def _rewrite_known_hashes(d: dict[int, Any]) -> dict[Any, Any]:
    # For records where the key is a 2-tuple (text, Value), Candid actually
    # encodes it as a record with field hashes 0 and 1. We leave those as-is
    # (the caller knows the shape). This hook is a no-op for our use case.
    return d


def decode_candid_icrc3_value(blob: bytes) -> Any:
    """Decode a Candid-encoded ICRC-3 Value blob into plain Python values."""
    reader = Reader(blob)
    magic = reader.read(4)
    if magic != b"DIDL":
        raise ValueError(f"not a Candid blob (magic={magic!r})")

    n_types = reader.uleb()
    table = [_parse_type(reader) for _ in range(n_types)]

    n_args = reader.uleb()
    arg_types = [reader.sleb() for _ in range(n_args)]

    decoded = [_decode_value(reader, _resolve(ref, table), table) for ref in arg_types]
    return decoded[0] if len(decoded) == 1 else decoded


def flatten_icrc3_value(v: Any) -> Any:
    """Turn the nested {Map/Text/Nat/Blob/Array/Int: ...} form into plain Python."""
    if not isinstance(v, dict) or len(v) != 1:
        return v
    (tag, inner), = v.items()
    if tag == "Text":
        return inner
    if tag == "Nat" or tag == "Int":
        return inner
    if tag == "Blob":
        # Candid `blob` is `vec nat8`, so `inner` is a list[int].
        return bytes(inner) if isinstance(inner, list) else inner
    if tag == "Array":
        return [flatten_icrc3_value(x) for x in inner]
    if tag == "Map":
        # `inner` is a list of records with hash-keys 0 (text) and 1 (value).
        out = {}
        for entry in inner:
            if isinstance(entry, dict):
                key = entry.get(candid_hash("0")) or entry.get(0) or list(entry.values())[0]
                val = entry.get(candid_hash("1")) or entry.get(1) or list(entry.values())[1]
                out[key] = flatten_icrc3_value(val)
            else:
                k, val = entry
                out[k] = flatten_icrc3_value(val)
        return out
    return v


# ---------------------------------------------------------------------------
# Signature / certificate helpers.
# ---------------------------------------------------------------------------


def summarize_signature(blob: bytes) -> dict[str, Any]:
    """Return a high-level summary of the IC signature envelope."""
    sig = cbor2.loads(blob)
    out: dict[str, Any] = {"keys": list(sig.keys())}

    cert = sig.get("certificate")
    if cert is not None:
        out["certificate_size"] = len(cert)
        try:
            parsed = cbor2.loads(cert)
            if isinstance(parsed, dict):
                out["certificate_fields"] = list(parsed.keys())
        except Exception:
            pass

    tree_sig = sig.get("signature")
    if isinstance(tree_sig, (bytes, bytearray)):
        out["tree_signature_hex"] = tree_sig.hex()

    deleg = sig.get("delegation")
    if isinstance(deleg, dict):
        out["delegation"] = {
            "subnet_id_hex": deleg.get("subnet_id", b"").hex(),
            "certificate_size": len(deleg.get("certificate", b"")),
        }

    return out


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def _json_default(obj):
    if isinstance(obj, (bytes, bytearray)):
        return {"_bytes_hex": obj.hex(), "_len": len(obj)}
    raise TypeError(f"not serializable: {type(obj)}")


def decode_blob(payload: dict[str, str]) -> dict[str, Any]:
    data_b64 = payload["data"]
    sig_b64 = payload["signature"]

    data_bytes = b64decode(data_b64)
    sig_bytes = b64decode(sig_b64)

    raw_value = decode_candid_icrc3_value(data_bytes)
    attributes = flatten_icrc3_value(raw_value)

    return {
        "attributes": attributes,
        "signature": summarize_signature(sig_bytes),
        "sizes": {"data": len(data_bytes), "signature": len(sig_bytes)},
    }


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2

    src = argv[1]
    raw = sys.stdin.read() if src == "-" else open(src, "r", encoding="utf-8").read()
    payload = json.loads(raw)
    decoded = decode_blob(payload)
    print(json.dumps(decoded, indent=2, default=_json_default, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
