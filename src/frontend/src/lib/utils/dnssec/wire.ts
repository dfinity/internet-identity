/**
 * DNS wire-format primitives — encode/decode names, build queries,
 * parse responses. Just enough to support the DNSSEC chain walk in
 * `chain.ts`; not a general-purpose DNS library.
 *
 * RFC references:
 * - 1035 §3 — message format, names, header, question, RR.
 * - 6891 §6.1 — EDNS(0) OPT pseudo-RR (carries the DO bit).
 * - 4034 §3 — RRSIG record.
 */

// ---------------------------------------------------------------------------
// Record-type constants
// ---------------------------------------------------------------------------

/** CNAME record (RFC 1035 §3.3.1). RDATA is a single DNS name. */
const TYPE_CNAME = 5;

// ---------------------------------------------------------------------------
// DNS name encoding
// ---------------------------------------------------------------------------

/**
 * Encode a textual DNS name (e.g. `"example.com"`) as wire-format
 * bytes: a sequence of length-prefixed labels terminated by a zero
 * length octet. Returns the canonical lowercased form so what we
 * submit to the canister matches the bytes that were actually
 * signed by the upstream zone.
 *
 * Rejects names with empty labels (`".x"`, `"x.."`) and labels
 * longer than 63 octets per RFC 1035 §2.3.4.
 */
export function encodeDnsName(name: string): Uint8Array {
  const trimmed = name.replace(/\.$/, "").toLowerCase();
  // Root: a single zero octet.
  if (trimmed.length === 0) {
    return new Uint8Array([0]);
  }
  const labels = trimmed.split(".");
  const out: number[] = [];
  for (const label of labels) {
    if (label.length === 0) {
      throw new Error(`empty label in DNS name "${name}"`);
    }
    if (label.length > 63) {
      throw new Error(`label "${label}" exceeds 63 bytes (RFC 1035 §2.3.4)`);
    }
    out.push(label.length);
    for (let i = 0; i < label.length; i++) {
      out.push(label.charCodeAt(i));
    }
  }
  out.push(0);
  return Uint8Array.from(out);
}

/**
 * Decode a DNS name starting at `offset` in `buf`, following RFC
 * 1035 §4.1.4 message-compression pointers. Returns the decoded
 * textual form (lowercased, with trailing dot stripped) plus the
 * offset immediately after the name in the original message.
 *
 * `buf` is the entire DNS message, not just the name slice — that's
 * required because compression pointers reference back to anywhere
 * in the message.
 */
export function decodeDnsName(
  buf: Uint8Array,
  offset: number,
): { name: string; nextOffset: number } {
  const labels: string[] = [];
  let cursor = offset;
  let jumped = false;
  let firstJumpReturn = -1;
  // Bound iterations to avoid infinite-loop on malicious pointers.
  for (let safety = 0; safety < 256; safety++) {
    if (cursor >= buf.length) {
      throw new Error("DNS name runs past end of message");
    }
    const len = buf[cursor];
    if (len === 0) {
      cursor++;
      break;
    }
    if ((len & 0xc0) === 0xc0) {
      // Compression pointer: high two bits set, next 14 bits are
      // the offset of the rest of the name.
      if (cursor + 1 >= buf.length) {
        throw new Error("truncated DNS compression pointer");
      }
      const ptr = ((len & 0x3f) << 8) | buf[cursor + 1];
      if (!jumped) {
        firstJumpReturn = cursor + 2;
        jumped = true;
      }
      cursor = ptr;
      continue;
    }
    if ((len & 0xc0) !== 0) {
      throw new Error(`reserved DNS label type bits 0x${len.toString(16)}`);
    }
    if (cursor + 1 + len > buf.length) {
      throw new Error("DNS label runs past end of message");
    }
    const labelBytes = buf.subarray(cursor + 1, cursor + 1 + len);
    labels.push(String.fromCharCode(...labelBytes).toLowerCase());
    cursor += 1 + len;
  }
  return {
    name: labels.join("."),
    nextOffset: jumped ? firstJumpReturn : cursor,
  };
}

/**
 * Same as `decodeDnsName` but also returns the *raw on-the-wire
 * uncompressed bytes* of the name (length-prefixed labels + null
 * terminator). The canister-side verifier hashes RRSIG signed-data
 * over this uncompressed form, so we need it as a side-channel for
 * each owner-name we'll bundle.
 */
export function decodeDnsNameWithBytes(
  buf: Uint8Array,
  offset: number,
): { name: string; nextOffset: number; bytes: Uint8Array } {
  const { name, nextOffset } = decodeDnsName(buf, offset);
  return { name, nextOffset, bytes: encodeDnsName(name) };
}

// ---------------------------------------------------------------------------
// DNS query construction
// ---------------------------------------------------------------------------

/**
 * Build a single-question DNS query suitable for DoH wire-format
 * (POST `application/dns-message`). Sets RD=1 (recursion desired)
 * and includes an EDNS(0) OPT record with DO=1 (DNSSEC OK) so the
 * resolver returns the matching RRSIG records alongside the answer.
 *
 * `id` is randomized to make accidental cross-talk impossible — the
 * DoH provider is responsible for matching responses, but we still
 * follow the protocol.
 */
export function buildDnsQuery(qname: string, qtype: number): Uint8Array {
  const id = Math.floor(Math.random() * 0x10000);
  const qnameBytes = encodeDnsName(qname);

  // Header (12 bytes). RD=1, all other flags zero. QDCOUNT=1,
  // ANCOUNT=0, NSCOUNT=0, ARCOUNT=1 (the OPT pseudo-RR).
  const header = new Uint8Array(12);
  header[0] = (id >> 8) & 0xff;
  header[1] = id & 0xff;
  header[2] = 0x01; // QR=0, OP=0, AA=0, TC=0, RD=1
  header[3] = 0x00; // RA=0, Z=0, RCODE=0
  header[4] = 0x00;
  header[5] = 0x01; // QDCOUNT=1
  header[6] = 0x00;
  header[7] = 0x00; // ANCOUNT=0
  header[8] = 0x00;
  header[9] = 0x00; // NSCOUNT=0
  header[10] = 0x00;
  header[11] = 0x01; // ARCOUNT=1

  // Question: qname | qtype(2) | qclass(2)
  const question = new Uint8Array(qnameBytes.length + 4);
  question.set(qnameBytes, 0);
  question[qnameBytes.length] = (qtype >> 8) & 0xff;
  question[qnameBytes.length + 1] = qtype & 0xff;
  question[qnameBytes.length + 2] = 0x00;
  question[qnameBytes.length + 3] = 0x01; // class IN

  // EDNS(0) OPT pseudo-RR (RFC 6891 §6.1):
  //   name = root (0x00)
  //   type = 41 (OPT)
  //   class = max UDP payload (4096)
  //   TTL = ext-rcode(0) | version(0) | DO=1 | Z=0
  //   rdlength = 0
  // 11 bytes total.
  const opt = new Uint8Array([
    0x00, // root name
    0x00,
    0x29, // type 41 (OPT)
    0x10,
    0x00, // class 4096
    0x00,
    0x00,
    0x80,
    0x00, // TTL with DO bit set
    0x00,
    0x00, // rdlength 0
  ]);

  const out = new Uint8Array(header.length + question.length + opt.length);
  out.set(header, 0);
  out.set(question, header.length);
  out.set(opt, header.length + question.length);
  return out;
}

// ---------------------------------------------------------------------------
// DNS response parsing
// ---------------------------------------------------------------------------

/**
 * One resource record. `rdata` is the raw on-the-wire RDATA bytes
 * — the canonical form per RFC 4034 §6.2 for the record types
 * we care about (DNSKEY, DS, RRSIG, TXT have no name compression
 * inside their RDATA). `rdataAbsolute` is the same bytes' position
 * in the original DNS message, useful when the caller needs to
 * replay them out-of-band.
 */
export interface DnsRR {
  /** Owner name in textual form, lowercased. */
  name: string;
  /** Owner name in canonical wire form (length-prefixed labels). */
  nameBytes: Uint8Array;
  /** Record type (16=TXT, 43=DS, 46=RRSIG, 48=DNSKEY, …). */
  type: number;
  /** Class (1=IN). */
  class_: number;
  /** TTL (seconds). */
  ttl: number;
  /** Raw RDATA bytes (canonical for the types we handle). */
  rdata: Uint8Array;
}

export interface DnsMessage {
  rcode: number;
  questions: { name: string; type: number; class_: number }[];
  answers: DnsRR[];
  authorities: DnsRR[];
  additionals: DnsRR[];
}

/**
 * Parse a DNS message returned by a DoH wire-format endpoint.
 * Returns the answer / authority / additional sections; each RR's
 * `rdata` is captured as raw bytes from the message (no
 * decompression — the record types we use in DNSSEC don't have
 * compressed names in their RDATA).
 */
export function parseDnsMessage(buf: Uint8Array): DnsMessage {
  if (buf.length < 12) {
    throw new Error("DNS message shorter than 12-byte header");
  }
  const flags = (buf[2] << 8) | buf[3];
  const rcode = flags & 0x0f;
  const qdcount = (buf[4] << 8) | buf[5];
  const ancount = (buf[6] << 8) | buf[7];
  const nscount = (buf[8] << 8) | buf[9];
  const arcount = (buf[10] << 8) | buf[11];

  let cursor = 12;
  const questions: DnsMessage["questions"] = [];
  for (let i = 0; i < qdcount; i++) {
    const { name, nextOffset } = decodeDnsName(buf, cursor);
    cursor = nextOffset;
    if (cursor + 4 > buf.length) {
      throw new Error("truncated question");
    }
    const type = (buf[cursor] << 8) | buf[cursor + 1];
    const class_ = (buf[cursor + 2] << 8) | buf[cursor + 3];
    cursor += 4;
    questions.push({ name, type, class_ });
  }

  const readRRs = (n: number): DnsRR[] => {
    const out: DnsRR[] = [];
    for (let i = 0; i < n; i++) {
      const decoded = decodeDnsNameWithBytes(buf, cursor);
      cursor = decoded.nextOffset;
      if (cursor + 10 > buf.length) {
        throw new Error("truncated RR header");
      }
      const type = (buf[cursor] << 8) | buf[cursor + 1];
      const class_ = (buf[cursor + 2] << 8) | buf[cursor + 3];
      const ttl =
        ((buf[cursor + 4] << 24) >>> 0) |
        ((buf[cursor + 5] << 16) >>> 0) |
        ((buf[cursor + 6] << 8) >>> 0) |
        (buf[cursor + 7] >>> 0);
      const rdlength = (buf[cursor + 8] << 8) | buf[cursor + 9];
      cursor += 10;
      if (cursor + rdlength > buf.length) {
        throw new Error("truncated RDATA");
      }
      // RFC 4034 §6.2: RDATA containing embedded DNS names must be
      // in canonical (uncompressed) form for DNSSEC signing. CNAME
      // RDATA may use message compression pointers, which only
      // resolve against the full DNS message — and the canister-
      // side verifier hashes RRSIG signed-data over the canonical
      // bytes. Decode against `buf` (handles pointers) and re-encode
      // uncompressed; for other types the raw rdata is already
      // canonical (TXT, A, AAAA, RRSIG, DNSKEY, DS, …).
      let rdata: Uint8Array;
      if (type === TYPE_CNAME) {
        const { name } = decodeDnsName(buf, cursor);
        rdata = encodeDnsName(name);
      } else {
        rdata = buf.subarray(cursor, cursor + rdlength).slice();
      }
      cursor += rdlength;
      out.push({
        name: decoded.name,
        nameBytes: decoded.bytes,
        type,
        class_,
        ttl: ttl >>> 0,
        rdata,
      });
    }
    return out;
  };

  const answers = readRRs(ancount);
  const authorities = readRRs(nscount);
  const additionals = readRRs(arcount);

  return { rcode, questions, answers, authorities, additionals };
}
