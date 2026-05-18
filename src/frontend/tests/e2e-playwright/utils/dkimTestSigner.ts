/**
 * In-test DKIM signer (relaxed/relaxed, RSA-SHA256), mirroring the
 * canister-side verifier and the Rust `dkim_signer` module in
 * `src/internet_identity/tests/integration/email_recovery.rs`.
 *
 * What it produces:
 *
 * - `TestSigner` — generates a fresh RSA-2048 keypair on
 *   construction. The matching public key is published as a DKIM
 *   TXT record via `publicTxtRecord()` (which the DoH route
 *   interceptor serves), and the private key signs each
 *   `SmtpRequest` via `signEmail()`.
 *
 * Unlike the DNSSEC signer, the DKIM keypair is *not* deterministic
 * across runs: each test gets its own pair. That's fine because the
 * DNSSEC chain is regenerated per-test and the canister's DKIM
 * verifier looks up the key out of the DNSSEC TXT it just verified,
 * not out of a hardcoded anchor.
 */

import type { SmtpRequest } from "$lib/generated/internet_identity_types";

export interface SignedEmailParams {
  from: string;
  to: string;
  subject: string;
  body: Uint8Array;
  /** Seconds since the epoch — goes into the `t=` tag. */
  timestamp: number;
}

export interface SignedEmail {
  request: SmtpRequest;
}

const DKIM_DATE = "Mon, 5 May 2026 12:00:00 +0000";

export class TestSigner {
  readonly #domain: string;
  readonly #selector: string;
  readonly #privateKey: CryptoKey;
  readonly #publicKeySpki: Uint8Array;

  private constructor(
    domain: string,
    selector: string,
    privateKey: CryptoKey,
    publicKeySpki: Uint8Array,
  ) {
    this.#domain = domain;
    this.#selector = selector;
    this.#privateKey = privateKey;
    this.#publicKeySpki = publicKeySpki;
  }

  static async create(domain: string, selector: string): Promise<TestSigner> {
    // 2048-bit RSA. The verifier accepts >= 1024 bits per design §5.6.
    const pair = (await crypto.subtle.generateKey(
      {
        name: "RSASSA-PKCS1-v1_5",
        modulusLength: 2048,
        publicExponent: new Uint8Array([0x01, 0x00, 0x01]),
        hash: "SHA-256",
      },
      true,
      ["sign", "verify"],
    )) as CryptoKeyPair;
    const spki = new Uint8Array(
      await crypto.subtle.exportKey("spki", pair.publicKey),
    );
    return new TestSigner(domain, selector, pair.privateKey, spki);
  }

  /**
   * `v=DKIM1; k=rsa; p={base64-SPKI-DER}` — the shape the canister-side
   * verifier expects (X.509 SubjectPublicKeyInfo per RFC 5280 §4.1, NOT
   * PKCS#1 RSAPublicKey). Real DKIM records publish keys this way too.
   */
  publicTxtRecord(): Uint8Array {
    const pB64 = base64Encode(this.#publicKeySpki);
    return new TextEncoder().encode(`v=DKIM1; k=rsa; p=${pB64}`);
  }

  async signEmail(params: SignedEmailParams): Promise<SignedEmail> {
    const msgId = `<test-${params.timestamp}@${this.#domain}>`;

    // 1. Body hash.
    const canonBody = relaxedBody(params.body);
    const bh = new Uint8Array(await crypto.subtle.digest("SHA-256", canonBody));
    const bhB64 = base64Encode(bh);

    // 2. The DKIM-Signature header value with `b=` empty. The
    //    signature input includes this (with the empty `b=`) so the
    //    order matters: build the rest first, leave `b=` last.
    const sigValueNoB =
      `v=1; a=rsa-sha256; c=relaxed/relaxed; d=${this.#domain}; s=${this.#selector}; ` +
      `t=${params.timestamp}; h=From:To:Subject:Date:Message-ID; ` +
      `bh=${bhB64}; b=`;

    // 3. Hash input: canonicalized signed headers + canonicalized
    //    DKIM-Signature header (with empty b=, no trailing CRLF).
    const headerParts: Uint8Array[] = [
      relaxedHeader("From", params.from),
      relaxedHeader("To", params.to),
      relaxedHeader("Subject", params.subject),
      relaxedHeader("Date", DKIM_DATE),
      relaxedHeader("Message-ID", msgId),
    ];
    let canonDkim = relaxedHeader("DKIM-Signature", sigValueNoB);
    // Strip trailing CRLF per RFC 6376 §3.7.
    if (
      canonDkim.length >= 2 &&
      canonDkim[canonDkim.length - 2] === 0x0d &&
      canonDkim[canonDkim.length - 1] === 0x0a
    ) {
      canonDkim = canonDkim.subarray(0, canonDkim.length - 2);
    }
    headerParts.push(canonDkim);
    const hashInput = concatBytes(headerParts);

    // 4. Sign.
    const sig = new Uint8Array(
      await crypto.subtle.sign(
        "RSASSA-PKCS1-v1_5",
        this.#privateKey,
        hashInput,
      ),
    );
    const sigB64 = base64Encode(sig);

    // 5. Final DKIM-Signature header value.
    const sigValueFinal = `${sigValueNoB}${sigB64}`;

    // 6. Build the SmtpRequest.
    const fromUser = params.from.split("@")[0] ?? "alice";
    const fromDomainRaw = params.from.split("@").slice(1).join("@");
    const fromDomain = fromDomainRaw === "" ? this.#domain : fromDomainRaw;
    const toUser = params.to.split("@")[0] ?? "register";
    const toDomainRaw = params.to.split("@").slice(1).join("@");
    const toDomain = toDomainRaw === "" ? "id.ai" : toDomainRaw;

    const request: SmtpRequest = {
      envelope: [
        {
          from: { user: fromUser, domain: fromDomain },
          to: { user: toUser, domain: toDomain },
        },
      ],
      message: [
        {
          headers: [
            { name: "DKIM-Signature", value: sigValueFinal },
            { name: "From", value: params.from },
            { name: "To", value: params.to },
            { name: "Subject", value: params.subject },
            { name: "Date", value: DKIM_DATE },
            { name: "Message-ID", value: msgId },
          ],
          body: params.body,
        },
      ],
      gateway_flags: [],
    };
    return { request };
  }
}

// ---------------------------------------------------------------------
// Relaxed canonicalization — mirrors `crate::dkim::canonicalize` and
// the Rust integration test's `relaxed_*` helpers.
// ---------------------------------------------------------------------

/** Apply RFC 6376 §3.4.2 to a single header. */
function relaxedHeader(name: string, value: string): Uint8Array {
  const lowerName = name.toLowerCase();
  const canonValue = relaxedHeaderValue(value);
  const enc = new TextEncoder();
  return concatBytes([
    enc.encode(lowerName),
    new Uint8Array([0x3a]), // ':'
    enc.encode(canonValue),
    new Uint8Array([0x0d, 0x0a]),
  ]);
}

function relaxedHeaderValue(value: string): string {
  const bytes = new TextEncoder().encode(value);
  const out: number[] = [];
  let i = 0;
  let lastWasSp = true;
  while (i < bytes.length) {
    const b = bytes[i];
    // Unfold: CRLF + WSP collapses to a single space (handled below).
    if (
      b === 0x0d &&
      i + 2 < bytes.length &&
      bytes[i + 1] === 0x0a &&
      isWsp(bytes[i + 2])
    ) {
      i += 2;
      continue;
    }
    if (isWsp(b)) {
      if (!lastWasSp) {
        out.push(0x20);
        lastWasSp = true;
      }
      i += 1;
      continue;
    }
    out.push(b);
    lastWasSp = false;
    i += 1;
  }
  if (out.length > 0 && out[out.length - 1] === 0x20) out.pop();
  return new TextDecoder().decode(Uint8Array.from(out));
}

function isWsp(b: number): boolean {
  return b === 0x20 || b === 0x09;
}

/** Apply RFC 6376 §3.4.4 to the body. */
function relaxedBody(body: Uint8Array): Uint8Array<ArrayBuffer> {
  const lines: Uint8Array[] = [];
  let start = 0;
  while (start < body.length) {
    let end = start;
    let hadCrlf = false;
    while (end < body.length) {
      if (
        body[end] === 0x0d &&
        end + 1 < body.length &&
        body[end + 1] === 0x0a
      ) {
        hadCrlf = true;
        break;
      }
      end += 1;
    }
    lines.push(canonBodyLine(body.subarray(start, end)));
    start = hadCrlf ? end + 2 : end;
  }
  while (lines.length > 0 && lines[lines.length - 1].length === 0) {
    lines.pop();
  }
  if (lines.length === 0) {
    const empty = new Uint8Array(2);
    empty[0] = 0x0d;
    empty[1] = 0x0a;
    return empty;
  }
  const parts: Uint8Array[] = [];
  for (const line of lines) {
    parts.push(line);
    parts.push(new Uint8Array([0x0d, 0x0a]));
  }
  return concatBytes(parts);
}

function canonBodyLine(line: Uint8Array): Uint8Array {
  const out: number[] = [];
  let lastWasSp = false;
  for (let i = 0; i < line.length; i++) {
    const b = line[i];
    if (isWsp(b)) {
      if (!lastWasSp) {
        out.push(0x20);
        lastWasSp = true;
      }
    } else {
      out.push(b);
      lastWasSp = false;
    }
  }
  if (out.length > 0 && out[out.length - 1] === 0x20) out.pop();
  return Uint8Array.from(out);
}

function concatBytes(parts: Uint8Array[]): Uint8Array<ArrayBuffer> {
  const total = parts.reduce((acc, p) => acc + p.length, 0);
  const out = new Uint8Array(total);
  let off = 0;
  for (const p of parts) {
    out.set(p, off);
    off += p.length;
  }
  return out;
}

function base64Encode(bytes: Uint8Array): string {
  // Browser-portable base64 — Buffer isn't available in the Playwright
  // page context if this signer ever moves there.
  let bin = "";
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin);
}
