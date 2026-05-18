#!/usr/bin/env node
/**
 * Emit the `dnssec_config` candid record literal whose root anchor
 * matches the deterministic root key derived by the e2e DNSSEC
 * signer at `dnssecTestSigner.ts`.
 *
 * The seed (`[1u8; 32]`) is the single source of truth — both the TS
 * signer and this script derive the public key, key tag, and SHA-256
 * digest from it. Any change to the seed must be made in
 * `dnssecTestSigner.ts`; rerunning this script then produces the
 * matching anchor for `src/internet_identity/local_test_arg.did`.
 *
 * Usage:
 *   node src/frontend/tests/e2e-playwright/utils/renderDnssecTestAnchor.mjs
 *
 * Output: a single line containing
 *   opt opt record { root_anchors = vec { record { ... } } }
 * suitable for splicing into `icp canister install --args` or
 * pasting into `local_test_arg.did`.
 */

import { ed25519 } from "@noble/curves/ed25519";

// MUST match `ROOT_SEED` in dnssecTestSigner.ts.
const ROOT_SEED = new Uint8Array(32).fill(1);
const ALG_ED25519 = 15;
const PROTOCOL_DNSSEC = 3;
const FLAGS_KSK_ZONE = 0x0001 | 0x0100;
const DIGEST_TYPE_SHA256 = 2;

function dnskeyRdata(pubkey) {
  const out = new Uint8Array(36);
  out[0] = (FLAGS_KSK_ZONE >> 8) & 0xff;
  out[1] = FLAGS_KSK_ZONE & 0xff;
  out[2] = PROTOCOL_DNSSEC;
  out[3] = ALG_ED25519;
  out.set(pubkey, 4);
  return out;
}

function dnskeyKeyTag(rdata) {
  let acc = 0;
  for (let i = 0; i < rdata.length; i++) {
    acc += (i & 1) === 0 ? rdata[i] << 8 : rdata[i];
    acc = acc >>> 0;
  }
  acc += (acc >>> 16) & 0xffff;
  return acc & 0xffff;
}

const pubkey = ed25519.getPublicKey(ROOT_SEED);
const rdata = dnskeyRdata(pubkey);
const keyTag = dnskeyKeyTag(rdata);

// Root owner is "." → wire form is a single zero byte; canonical
// form (lowercased labels) is the same.
const ownerCanon = new Uint8Array([0]);
const buf = new Uint8Array(ownerCanon.length + rdata.length);
buf.set(ownerCanon, 0);
buf.set(rdata, ownerCanon.length);
const digest = new Uint8Array(await crypto.subtle.digest("SHA-256", buf));

const blob = Array.from(digest)
  .map((x) => `\\${x.toString(16).padStart(2, "0")}`)
  .join("");

const candid =
  `opt opt record { root_anchors = vec { record { ` +
  `key_tag = ${keyTag} : nat16; ` +
  `algorithm = ${ALG_ED25519} : nat8; ` +
  `digest_type = ${DIGEST_TYPE_SHA256} : nat8; ` +
  `digest = blob "${blob}" ` +
  `} } }`;

process.stdout.write(candid + "\n");
