// Minimal Candid decoder for the ICRC-3 Value type returned by Internet
// Identity's `prepare_icrc3_attributes` / `get_icrc3_attributes` pair.
//
// An ICRC-3 Value is a recursive variant:
//   variant { Nat : nat; Int : int; Blob : blob; Text : text;
//             Array : vec Value; Map : vec record { text; Value } }
// The attribute payload is always a `Map` at the top level.

export type Icrc3Value =
  | { Nat: bigint }
  | { Int: bigint }
  | { Blob: Uint8Array }
  | { Text: string }
  | { Array: Icrc3Value[] }
  | { Map: Array<[string, Icrc3Value]> };

const PRIM: Record<number, string> = {
  [-1]: "null",
  [-2]: "bool",
  [-3]: "nat",
  [-4]: "int",
  [-5]: "nat8",
  [-6]: "nat16",
  [-7]: "nat32",
  [-8]: "nat64",
  [-9]: "int8",
  [-10]: "int16",
  [-11]: "int32",
  [-12]: "int64",
  [-15]: "text",
  [-16]: "reserved",
  [-17]: "empty",
};

const T_OPT = -18;
const T_VEC = -19;
const T_RECORD = -20;
const T_VARIANT = -21;

type CType =
  | { kind: "prim"; name: string }
  | { kind: "opt"; inner: number }
  | { kind: "vec"; inner: number }
  | { kind: "record"; fields: Array<[number, number]> }
  | { kind: "variant"; fields: Array<[number, number]> };

class Reader {
  private pos = 0;
  constructor(private readonly buf: Uint8Array) {}

  read(n: number): Uint8Array {
    if (this.pos + n > this.buf.length) {
      throw new Error(`unexpected EOF (need ${n})`);
    }
    const out = this.buf.subarray(this.pos, this.pos + n);
    this.pos += n;
    return out;
  }

  uleb(): bigint {
    let result = BigInt(0);
    let shift = BigInt(0);
    const seven = BigInt(7);
    for (;;) {
      const b = this.read(1)[0];
      result |= BigInt(b & 0x7f) << shift;
      if ((b & 0x80) === 0) return result;
      shift += seven;
    }
  }

  sleb(): bigint {
    let result = BigInt(0);
    let shift = BigInt(0);
    const seven = BigInt(7);
    let byte = 0;
    for (;;) {
      byte = this.read(1)[0];
      result |= BigInt(byte & 0x7f) << shift;
      shift += seven;
      if ((byte & 0x80) === 0) break;
    }
    if (byte & 0x40) {
      result |= -(BigInt(1) << shift);
    }
    return result;
  }
}

// Candid field-name hash per spec: sum(b * 223^i) mod 2^32.
function candidHash(name: string): number {
  let h = BigInt(0);
  const mult = BigInt(223);
  const mask = BigInt(0xffffffff);
  const bytes = new TextEncoder().encode(name);
  for (const b of bytes) {
    h = (h * mult + BigInt(b)) & mask;
  }
  return Number(h);
}

const VARIANT_TAGS: Record<
  number,
  "Nat" | "Int" | "Blob" | "Text" | "Array" | "Map"
> = {
  [candidHash("Nat")]: "Nat",
  [candidHash("Int")]: "Int",
  [candidHash("Blob")]: "Blob",
  [candidHash("Text")]: "Text",
  [candidHash("Array")]: "Array",
  [candidHash("Map")]: "Map",
};

function parseType(reader: Reader): CType {
  const op = Number(reader.sleb());
  if (op < 0 && PRIM[op] !== undefined) {
    return { kind: "prim", name: PRIM[op] };
  }
  if (op === T_OPT) return { kind: "opt", inner: Number(reader.sleb()) };
  if (op === T_VEC) return { kind: "vec", inner: Number(reader.sleb()) };
  if (op === T_RECORD || op === T_VARIANT) {
    const n = Number(reader.uleb());
    const fields: Array<[number, number]> = [];
    for (let i = 0; i < n; i++) {
      fields.push([Number(reader.uleb()), Number(reader.sleb())]);
    }
    return { kind: op === T_RECORD ? "record" : "variant", fields };
  }
  throw new Error(`unsupported Candid type opcode ${op}`);
}

function resolve(ref: number, table: CType[]): CType {
  if (ref < 0) {
    if (PRIM[ref] === undefined) {
      throw new Error(`unknown primitive ${ref}`);
    }
    return { kind: "prim", name: PRIM[ref] };
  }
  return table[ref];
}

function decodePrim(reader: Reader, name: string): unknown {
  switch (name) {
    case "nat":
      return reader.uleb();
    case "int":
      return reader.sleb();
    case "text": {
      const n = Number(reader.uleb());
      return new TextDecoder().decode(reader.read(n));
    }
    case "bool":
      return reader.read(1)[0] !== 0;
    case "null":
    case "reserved":
      return null;
    case "nat8":
      return reader.read(1)[0];
  }
  throw new Error(`primitive ${name} not implemented`);
}

function decodeValue(reader: Reader, typ: CType, table: CType[]): unknown {
  if (typ.kind === "prim") return decodePrim(reader, typ.name);
  if (typ.kind === "vec") {
    const n = Number(reader.uleb());
    const inner = resolve(typ.inner, table);
    // Optimize vec nat8 -> Uint8Array (i.e. Candid `blob`).
    if (inner.kind === "prim" && inner.name === "nat8") {
      return new Uint8Array(reader.read(n));
    }
    const out: unknown[] = [];
    for (let i = 0; i < n; i++) out.push(decodeValue(reader, inner, table));
    return out;
  }
  if (typ.kind === "opt") {
    const flag = reader.read(1)[0];
    if (flag === 0) return null;
    return decodeValue(reader, resolve(typ.inner, table), table);
  }
  if (typ.kind === "record") {
    const out: Record<number, unknown> = {};
    for (const [hash, ref] of typ.fields) {
      out[hash] = decodeValue(reader, resolve(ref, table), table);
    }
    return out;
  }
  if (typ.kind === "variant") {
    const idx = Number(reader.uleb());
    const [hash, ref] = typ.fields[idx];
    const tag = VARIANT_TAGS[hash] ?? `hash_${hash}`;
    return { [tag]: decodeValue(reader, resolve(ref, table), table) };
  }
  throw new Error(`cannot decode ${(typ as CType).kind}`);
}

/** Decode a Candid-encoded ICRC-3 Value blob into its structured form. */
export function decodeIcrc3Value(blob: Uint8Array): Icrc3Value {
  const reader = new Reader(blob);
  const magic = reader.read(4);
  if (
    magic[0] !== 0x44 ||
    magic[1] !== 0x49 ||
    magic[2] !== 0x44 ||
    magic[3] !== 0x4c
  ) {
    throw new Error("not a Candid blob (missing DIDL magic)");
  }

  const nTypes = Number(reader.uleb());
  const table: CType[] = [];
  for (let i = 0; i < nTypes; i++) table.push(parseType(reader));

  const nArgs = Number(reader.uleb());
  if (nArgs !== 1) {
    throw new Error(`expected 1 argument, got ${nArgs}`);
  }
  const argRef = Number(reader.sleb());
  const decoded = decodeValue(reader, resolve(argRef, table), table) as unknown;

  return raw2Icrc3Value(decoded);
}

// Turn the intermediate decoder output (records keyed by field hash) into the
// nicely-typed Icrc3Value form. Map records use Candid tuple-field hashes 0/1.
function raw2Icrc3Value(v: unknown): Icrc3Value {
  if (v === null || typeof v !== "object") {
    throw new Error(`unexpected Value shape: ${typeof v}`);
  }
  const entries = Object.entries(v as Record<string, unknown>);
  if (entries.length !== 1) {
    throw new Error("expected variant with a single tag");
  }
  const [tag, inner] = entries[0];
  switch (tag) {
    case "Nat":
      return { Nat: inner as bigint };
    case "Int":
      return { Int: inner as bigint };
    case "Blob":
      return { Blob: inner as Uint8Array };
    case "Text":
      return { Text: inner as string };
    case "Array":
      return { Array: (inner as unknown[]).map(raw2Icrc3Value) };
    case "Map": {
      const pairs = inner as Array<Record<number, unknown>>;
      const out: Array<[string, Icrc3Value]> = pairs.map((entry) => {
        // Candid tuple `record { text; Value }` encodes fields with hashes 0 and 1.
        const key = entry[0] as string;
        const value = raw2Icrc3Value(entry[1]);
        return [key, value];
      });
      return { Map: out };
    }
    default:
      throw new Error(`unknown ICRC-3 variant: ${tag}`);
  }
}

/**
 * Best-effort conversion of an ICRC-3 Value tree into a plain JS value tree
 * suitable for human-readable display. Blobs become `Uint8Array` (render them
 * as hex), and Maps become plain objects.
 */
export function flattenIcrc3Value(v: Icrc3Value): unknown {
  if ("Nat" in v) return v.Nat;
  if ("Int" in v) return v.Int;
  if ("Text" in v) return v.Text;
  if ("Blob" in v) return v.Blob;
  if ("Array" in v) return v.Array.map(flattenIcrc3Value);
  const out: Record<string, unknown> = {};
  for (const [k, inner] of v.Map) out[k] = flattenIcrc3Value(inner);
  return out;
}

function toHex(bytes: Uint8Array): string {
  let s = "";
  for (const b of bytes) s += b.toString(16).padStart(2, "0");
  return s;
}

/** Format a decoded ICRC-3 attribute map as human-readable text. */
export function formatIcrc3Attributes(blob: Uint8Array): string {
  const value = decodeIcrc3Value(blob);
  const flat = flattenIcrc3Value(value);
  if (flat === null || typeof flat !== "object" || Array.isArray(flat)) {
    return JSON.stringify(flat, jsonReplacer, 2);
  }
  const entries = Object.entries(flat as Record<string, unknown>).sort(
    ([a], [b]) => a.localeCompare(b),
  );
  return entries.map(([k, v]) => `${k}: ${renderValue(v)}`).join("\n");
}

function renderValue(v: unknown): string {
  if (v instanceof Uint8Array) return `0x${toHex(v)}`;
  if (typeof v === "bigint") return v.toString();
  return JSON.stringify(v, jsonReplacer);
}

function jsonReplacer(_: string, value: unknown): unknown {
  if (typeof value === "bigint") return value.toString();
  if (value instanceof Uint8Array) return `0x${toHex(value)}`;
  return value;
}
