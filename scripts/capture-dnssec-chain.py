#!/usr/bin/env python3
"""Capture a DNSSEC chain from a public DoH resolver into a JSON file
shaped exactly like the canister-side `DnsProofBundle`.

Usage:
    capture-dnssec-chain.py <leaf-name> <leaf-rtype> <deepest-zone> [--out FILE]

The output JSON looks like this:

    {
      "_meta": {...},
      "root_dnskey": <SignedRRset>,
      "chain": [<DelegationLink>, ...],   # top-down: root→TLD→...→deepest_zone
      "leaf": <SignedRRset>,
    }

Where each <SignedRRset> is:

    {
      "name_hex": "<hex bytes of canonical wire form>",
      "rtype": 16,
      "rdata_hex": ["<hex>", "<hex>", ...],     # one per RR
      "ttl": 3600,
      "rrsig": {
        "type_covered": 16,
        "algorithm": 13,
        "labels": 3,
        "original_ttl": 3600,
        "expiration": 1799999999,
        "inception": 1799000000,
        "key_tag": 12345,
        "signer_name": "<hex>",
        "signature_hex": "<hex>"
      }
    }

The canister-side test loader deserializes this directly with serde. The
verifier then operates on the `DnsProofBundle` exactly as it would on
input from the live FE in production — so the test path exercises the
same code path as production from the boundary inward.

dnspython is used to do the wire-format DoH queries and decode the
responses into structured records; the script then extracts the RDATA
into canonical wire form and serializes.

Run with `apt-get install python3-dnspython` if dnspython is missing.
"""
from __future__ import annotations

import argparse
import io
import json
import sys
import time
from typing import Iterable, Optional

import dns.message
import dns.name
import dns.query
import dns.rdataset
import dns.rdatatype
import dns.rrset


def chain_zones(deepest_zone: str) -> list[dns.name.Name]:
    """Return the zone delegation walk top-down (root first)."""
    zone = dns.name.from_text(deepest_zone)
    zones: list[dns.name.Name] = []
    cur = zone
    while True:
        zones.append(cur)
        if cur == dns.name.root:
            break
        cur = cur.parent()
    zones.reverse()
    return zones


def doh_query(name: dns.name.Name, rtype: str, resolver: str) -> dns.message.Message:
    msg = dns.message.make_query(name, rtype, want_dnssec=True)
    return dns.query.https(msg, resolver, timeout=10)


def find_rrset(
    response: dns.message.Message, name: dns.name.Name, rtype: str
) -> tuple[dns.rrset.RRset, dns.rrset.RRset]:
    """Return (rrset, rrsig_rrset) from the answer section.

    Raises if either is missing.
    """
    rdtype = dns.rdatatype.from_text(rtype)
    rrset = response.find_rrset(response.answer, name, dns.rdataclass.IN, rdtype)
    rrsig = response.find_rrset(
        response.answer,
        name,
        dns.rdataclass.IN,
        dns.rdatatype.RRSIG,
        covers=rdtype,
    )
    return rrset, rrsig


def name_to_canonical_hex(name: dns.name.Name) -> str:
    """Canonical wire form per RFC 4034 §6.2: lowercased labels, no compression."""
    out = io.BytesIO()
    name.canonicalize().to_wire(out)
    return out.getvalue().hex()


def rdata_to_wire_hex(rd) -> str:
    out = io.BytesIO()
    rd.to_wire(out, canonicalize=True)
    return out.getvalue().hex()


def serialize_rrsig(rrsig_rdata) -> dict:
    return {
        "type_covered": int(rrsig_rdata.type_covered),
        "algorithm": int(rrsig_rdata.algorithm),
        "labels": int(rrsig_rdata.labels),
        "original_ttl": int(rrsig_rdata.original_ttl),
        "expiration": int(rrsig_rdata.expiration),
        "inception": int(rrsig_rdata.inception),
        "key_tag": int(rrsig_rdata.key_tag),
        "signer_name": name_to_canonical_hex(rrsig_rdata.signer),
        "signature_hex": rrsig_rdata.signature.hex(),
    }


def serialize_signed_rrset(
    rrset: dns.rrset.RRset, rrsig_rrset: dns.rrset.RRset
) -> dict:
    # Pick the first RRSIG covering the rrset. (DoH responses typically
    # carry one RRSIG per applicable DNSKEY, but for our purposes any one
    # signature that verifies is enough.)
    rrsig_rdata = list(rrsig_rrset)[0]
    rdata_hex = sorted(rdata_to_wire_hex(rd) for rd in rrset)
    return {
        "name_hex": name_to_canonical_hex(rrset.name),
        "rtype": int(rrset.rdtype),
        "rdata_hex": rdata_hex,
        "ttl": int(rrset.ttl),
        "rrsig": serialize_rrsig(rrsig_rdata),
    }


def capture(
    leaf: str, leaf_rtype: str, deepest_zone: str, resolver: str
) -> dict:
    zones = chain_zones(deepest_zone)

    # Root DNSKEY
    root = zones[0]
    root_resp = doh_query(root, "DNSKEY", resolver)
    root_rrset, root_rrsig = find_rrset(root_resp, root, "DNSKEY")
    root_dnskey = serialize_signed_rrset(root_rrset, root_rrsig)

    # Walk down
    chain = []
    for child in zones[1:]:
        ds_resp = doh_query(child, "DS", resolver)
        ds_rrset, ds_rrsig = find_rrset(ds_resp, child, "DS")
        dnskey_resp = doh_query(child, "DNSKEY", resolver)
        dnskey_rrset, dnskey_rrsig = find_rrset(dnskey_resp, child, "DNSKEY")
        chain.append({
            "child_ds": serialize_signed_rrset(ds_rrset, ds_rrsig),
            "child_dnskey": serialize_signed_rrset(dnskey_rrset, dnskey_rrsig),
        })
        time.sleep(0.05)

    # Leaf
    leaf_name = dns.name.from_text(leaf)
    leaf_resp = doh_query(leaf_name, leaf_rtype, resolver)
    leaf_rrset, leaf_rrsig = find_rrset(leaf_resp, leaf_name, leaf_rtype)
    leaf_signed = serialize_signed_rrset(leaf_rrset, leaf_rrsig)

    return {
        "_meta": {
            "leaf_name": leaf,
            "leaf_rtype": leaf_rtype,
            "deepest_zone": deepest_zone,
            "captured_at_unix": int(time.time()),
            "resolver": resolver,
        },
        "root_dnskey": root_dnskey,
        "chain": chain,
        "leaf": leaf_signed,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("leaf_name", help="e.g. cloudflare.com")
    parser.add_argument("leaf_rtype", help="e.g. TXT")
    parser.add_argument(
        "deepest_zone",
        help="The deepest signed zone the leaf is in (e.g. cloudflare.com)",
    )
    parser.add_argument("--out", default="-", help="Output file (default: stdout)")
    parser.add_argument(
        "--resolver",
        default="https://1.1.1.1/dns-query",
        help="DoH endpoint (default: https://1.1.1.1/dns-query)",
    )
    args = parser.parse_args()

    bundle = capture(
        args.leaf_name, args.leaf_rtype, args.deepest_zone, args.resolver
    )
    out = json.dumps(bundle, indent=2, sort_keys=True) + "\n"

    if args.out == "-":
        sys.stdout.write(out)
    else:
        with open(args.out, "w") as f:
            f.write(out)
        print(
            f"Captured root DNSKEY + {len(bundle['chain'])} chain links + "
            f"leaf → {args.out}",
            file=sys.stderr,
        )

    return 0


if __name__ == "__main__":
    sys.exit(main())
