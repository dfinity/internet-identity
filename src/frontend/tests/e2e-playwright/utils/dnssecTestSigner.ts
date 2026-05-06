/**
 * In-test DNSSEC chain builder, mirroring the canister-side
 * verifier's expectations and the Rust `dnssec_signer` module in
 * `src/internet_identity/tests/integration/email_recovery.rs`.
 *
 * What it produces:
 *
 * - `ROOT_ANCHOR` — the `DnssecRootAnchor` digest the canister
 *   should be deployed with so chains built here verify. The root
 *   keypair is derived from a fixed seed (`[1u8; 32]`) so the
 *   anchor stays stable across test runs and can be embedded in
 *   `local_test_arg.did`. Equivalent to the Rust integration
 *   test's `let root_key = ZoneKey::new(b"\x00".to_vec(), [1u8; 32])`.
 * - `buildChain(domain, selector, dkimTxt, dmarcTxt, nowSecs)` —
 *   single-link chain (root → leaf zone) signing TXT records at
 *   `<selector>._domainkey.<domain>` and (optionally)
 *   `_dmarc.<domain>`.
 *
 * The chain is sufficient for the DNSSEC happy-path. Multi-link
 * (root → TLD → domain) bundles are exercised in the canister's
 * Rust integration tests; the TS shape here only needs to round-
 * trip what the FE wizard's DNSSEC walker assembles.
 */

import { ed25519 } from "@noble/curves/ed25519";
import type {
  DnsProofBundle,
  DnssecRootAnchor,
  Rrsig,
  SignedRRset,
} from "$lib/generated/internet_identity_types";

const ALG_ED25519 = 15;
const PROTOCOL_DNSSEC = 3;
const FLAGS_KSK_ZONE = 0x0001 | 0x0100;
const TYPE_TXT = 16;
const TYPE_DS = 43;
const TYPE_DNSKEY = 48;
const CLASS_IN = 1;
const DIGEST_TYPE_SHA256 = 2;

// Fixed seed for the root keypair so the matching anchor digest is
// deterministic and we can hardcode it into local_test_arg.did.
// Same seed the Rust integration test uses for its in-test signer
// (`[1u8; 32]`).
const ROOT_SEED = new Uint8Array(32).fill(1);
// Leaf zone seed; arbitrary but distinct from the root.
const LEAF_ZONE_SEED = new Uint8Array(32).fill(2);

interface ZoneKey {
  seed: Uint8Array;
  publicKey: Uint8Array; // 32 bytes
  ownerName: Uint8Array; // wire form
}

function makeZoneKey(ownerName: Uint8Array, seed: Uint8Array): ZoneKey {
  return {
    seed,
    publicKey: ed25519.getPublicKey(seed),
    ownerName,
  };
}

function dnskeyRdata(zone: ZoneKey): Uint8Array {
  const out = new Uint8Array(36);
  out[0] = (FLAGS_KSK_ZONE >> 8) & 0xff;
  out[1] = FLAGS_KSK_ZONE & 0xff;
  out[2] = PROTOCOL_DNSSEC;
  out[3] = ALG_ED25519;
  out.set(zone.publicKey, 4);
  return out;
}

function dnskeyKeyTag(rdata: Uint8Array): number {
  let acc = 0;
  for (let i = 0; i < rdata.length; i++) {
    acc += (i & 1) === 0 ? rdata[i] << 8 : rdata[i];
    acc = acc >>> 0;
  }
  acc += (acc >>> 16) & 0xffff;
  return acc & 0xffff;
}

async function sha256(...parts: Uint8Array[]): Promise<Uint8Array> {
  const total = parts.reduce((acc, p) => acc + p.length, 0);
  const buf = new Uint8Array(total);
  let off = 0;
  for (const p of parts) {
    buf.set(p, off);
    off += p.length;
  }
  return new Uint8Array(await crypto.subtle.digest("SHA-256", buf));
}

function encodeDnsName(name: string): Uint8Array {
  const trimmed = name.replace(/\.$/, "").toLowerCase();
  if (trimmed.length === 0) {
    return new Uint8Array([0]);
  }
  const labels = trimmed.split(".");
  const out: number[] = [];
  for (const label of labels) {
    if (label.length === 0 || label.length > 63) {
      throw new Error(`invalid label "${label}" in "${name}"`);
    }
    out.push(label.length);
    for (let i = 0; i < label.length; i++) out.push(label.charCodeAt(i));
  }
  out.push(0);
  return Uint8Array.from(out);
}

function canonicalizeName(wire: Uint8Array): Uint8Array {
  const out = new Uint8Array(wire.length);
  let i = 0;
  while (i < wire.length) {
    const len = wire[i];
    out[i] = len;
    if (len === 0) {
      return out.subarray(0, i + 1);
    }
    for (let j = 1; j <= len; j++) {
      out[i + j] = toLowerByte(wire[i + j]);
    }
    i += 1 + len;
  }
  return out.subarray(0, i);
}

function toLowerByte(b: number): number {
  return b >= 0x41 && b <= 0x5a ? b + 0x20 : b;
}

function countLabels(wire: Uint8Array): number {
  let count = 0;
  let i = 0;
  while (i < wire.length) {
    const len = wire[i];
    if (len === 0) break;
    count++;
    i += 1 + len;
  }
  return count;
}

function packTxtRdata(text: Uint8Array): Uint8Array {
  // <len><bytes><len><bytes>… per RFC 6376 §3.6.2.2 character-strings.
  const chunks: number[] = [];
  for (let i = 0; i < text.length; i += 255) {
    const end = Math.min(i + 255, text.length);
    chunks.push(end - i);
    for (let j = i; j < end; j++) chunks.push(text[j]);
  }
  return Uint8Array.from(chunks);
}

function rrsigRdataForSigning(rrsig: Omit<Rrsig, "signature">): Uint8Array {
  const signerCanon = canonicalizeName(toBytes(rrsig.signer_name));
  const out = new Uint8Array(18 + signerCanon.length);
  const dv = new DataView(out.buffer);
  dv.setUint16(0, rrsig.type_covered);
  out[2] = rrsig.algorithm;
  out[3] = rrsig.labels;
  dv.setUint32(4, rrsig.original_ttl);
  dv.setUint32(8, rrsig.expiration);
  dv.setUint32(12, rrsig.inception);
  dv.setUint16(16, rrsig.key_tag);
  out.set(signerCanon, 18);
  return out;
}

function rrCanonical(
  nameCanonical: Uint8Array,
  rtype: number,
  originalTtl: number,
  rdata: Uint8Array,
): Uint8Array {
  const out = new Uint8Array(nameCanonical.length + 10 + rdata.length);
  out.set(nameCanonical, 0);
  const dv = new DataView(out.buffer, out.byteOffset);
  dv.setUint16(nameCanonical.length, rtype);
  dv.setUint16(nameCanonical.length + 2, CLASS_IN);
  dv.setUint32(nameCanonical.length + 4, originalTtl);
  dv.setUint16(nameCanonical.length + 8, rdata.length);
  out.set(rdata, nameCanonical.length + 10);
  return out;
}

function buildSignedData(
  ownerName: Uint8Array,
  rtype: number,
  rrsig: Omit<Rrsig, "signature">,
  rdatas: Uint8Array[],
): Uint8Array {
  const canonName = canonicalizeName(ownerName);
  // Sort RDATAs in canonical octet order per RFC 4034 §6.3.
  const sorted = [...rdatas].sort((a, b) => bytesCmp(a, b));
  const header = rrsigRdataForSigning(rrsig);
  let total = header.length;
  const rrs: Uint8Array[] = [];
  for (const rd of sorted) {
    const rr = rrCanonical(canonName, rtype, rrsig.original_ttl, rd);
    rrs.push(rr);
    total += rr.length;
  }
  const out = new Uint8Array(total);
  out.set(header, 0);
  let off = header.length;
  for (const rr of rrs) {
    out.set(rr, off);
    off += rr.length;
  }
  return out;
}

function bytesCmp(a: Uint8Array, b: Uint8Array): number {
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    if (a[i] !== b[i]) return a[i] - b[i];
  }
  return a.length - b.length;
}

function toBytes(v: Uint8Array | number[]): Uint8Array {
  return v instanceof Uint8Array ? v : Uint8Array.from(v);
}

function signRrset(
  signer: ZoneKey,
  ownerName: Uint8Array,
  rtype: number,
  rdatas: Uint8Array[],
  labels: number,
  inception: number,
  expiration: number,
): SignedRRset {
  const rrsigBase: Omit<Rrsig, "signature"> = {
    type_covered: rtype,
    algorithm: ALG_ED25519,
    labels,
    original_ttl: 3600,
    expiration,
    inception,
    key_tag: dnskeyKeyTag(dnskeyRdata(signer)),
    signer_name: signer.ownerName,
  };
  const signedData = buildSignedData(ownerName, rtype, rrsigBase, rdatas);
  const signature = ed25519.sign(signedData, signer.seed);
  return {
    name: ownerName,
    rtype,
    rdata: rdatas,
    ttl: 3600,
    rrsig: { ...rrsigBase, signature },
  };
}

async function dsRdataFor(child: ZoneKey): Promise<Uint8Array> {
  const dnskey = dnskeyRdata(child);
  const canon = canonicalizeName(child.ownerName);
  const digest = await sha256(canon, dnskey);
  const keyTag = dnskeyKeyTag(dnskey);
  const out = new Uint8Array(4 + digest.length);
  out[0] = (keyTag >> 8) & 0xff;
  out[1] = keyTag & 0xff;
  out[2] = ALG_ED25519;
  out[3] = DIGEST_TYPE_SHA256;
  out.set(digest, 4);
  return out;
}

async function makeAnchor(root: ZoneKey): Promise<DnssecRootAnchor> {
  const dnskey = dnskeyRdata(root);
  const canon = canonicalizeName(root.ownerName);
  const digest = await sha256(canon, dnskey);
  return {
    key_tag: dnskeyKeyTag(dnskey),
    algorithm: ALG_ED25519,
    digest_type: DIGEST_TYPE_SHA256,
    digest,
  };
}

/**
 * Build a single-link DNSSEC chain (root → leaf zone) for the
 * email-recovery flow. RRSIGs are valid from `nowSecs - 60` to
 * `nowSecs + 7d`.
 *
 * Returns the chain components separately so callers can compose
 * single-leaf bundles for the two-phase flow:
 * - `skeleton` (chain only, `leaf = []`) → for `prepare_add` /
 *   `prepare_delegation` (when `dmarcTxt` is `undefined`).
 * - `skeleton` with `leaf = [dmarcLeaf]` → for `prepare` when
 *   the test wants DMARC included.
 * - `skeleton` with `leaf = [dkimLeaf]` → for `submit_dkim_leaf`.
 *
 * The matching `anchor` must already be present in the canister's
 * `dnssec_config.root_anchors`.
 */
export async function buildChain(args: {
  domain: string;
  selector: string;
  dkimTxt: Uint8Array;
  dmarcTxt?: Uint8Array;
  nowSecs: number;
}): Promise<{
  anchor: DnssecRootAnchor;
  skeleton: DnsProofBundle;
  dkimLeaf: SignedRRset;
  dmarcLeaf: SignedRRset | undefined;
}> {
  const inception = Math.max(0, args.nowSecs - 60);
  const expiration = args.nowSecs + 7 * 24 * 3600;

  const rootKey = makeZoneKey(new Uint8Array([0]), ROOT_SEED);
  const zoneWire = encodeDnsName(args.domain);
  const zoneKey = makeZoneKey(zoneWire, LEAF_ZONE_SEED);

  const rootDnskey = signRrset(
    rootKey,
    rootKey.ownerName,
    TYPE_DNSKEY,
    [dnskeyRdata(rootKey)],
    0,
    inception,
    expiration,
  );

  const childDs = signRrset(
    rootKey,
    zoneWire,
    TYPE_DS,
    [await dsRdataFor(zoneKey)],
    countLabels(zoneWire),
    inception,
    expiration,
  );

  const childDnskey = signRrset(
    zoneKey,
    zoneWire,
    TYPE_DNSKEY,
    [dnskeyRdata(zoneKey)],
    countLabels(zoneWire),
    inception,
    expiration,
  );

  const dkimOwner = encodeDnsName(`${args.selector}._domainkey.${args.domain}`);
  const dkimLeaf = signRrset(
    zoneKey,
    dkimOwner,
    TYPE_TXT,
    [packTxtRdata(args.dkimTxt)],
    countLabels(dkimOwner),
    inception,
    expiration,
  );

  let dmarcLeaf: SignedRRset | undefined;
  if (args.dmarcTxt !== undefined) {
    const dmarcOwner = encodeDnsName(`_dmarc.${args.domain}`);
    dmarcLeaf = signRrset(
      zoneKey,
      dmarcOwner,
      TYPE_TXT,
      [packTxtRdata(args.dmarcTxt)],
      countLabels(dmarcOwner),
      inception,
      expiration,
    );
  }

  const anchor = await makeAnchor(rootKey);

  return {
    anchor,
    skeleton: {
      leaf: [],
      root_dnskey: rootDnskey,
      chain: [{ child_ds: childDs, child_dnskey: childDnskey }],
    },
    dkimLeaf,
    dmarcLeaf,
  };
}

/**
 * Compute the deterministic root anchor used by every test built
 * with this signer, *without* building a full chain. Useful for the
 * one-off scripts that emit `local_test_arg.did` so the canister
 * can be deployed with a matching `dnssec_config`.
 */
export function rootAnchor(): Promise<DnssecRootAnchor> {
  const rootKey = makeZoneKey(new Uint8Array([0]), ROOT_SEED);
  return makeAnchor(rootKey);
}

// Re-export some helpers so the DoH route interceptor can replay the
// exact bytes from the chain we just built.
export { encodeDnsName, canonicalizeName, dnskeyRdata, dnskeyKeyTag };
