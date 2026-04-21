// Decoder for the ICRC-3 Value type returned by Internet Identity's
// `prepare_icrc3_attributes` / `get_icrc3_attributes` pair.
//
// An ICRC-3 Value is a recursive variant:
//   variant { Nat : nat; Int : int; Blob : blob; Text : text;
//             Array : vec Value; Map : vec record { text; Value } }

import { IDL } from "@icp-sdk/core/candid";

export type Icrc3Value =
  | { Nat: bigint }
  | { Int: bigint }
  | { Blob: number[] }
  | { Text: string }
  | { Array: Icrc3Value[] }
  | { Map: Array<[string, Icrc3Value]> };

const Icrc3Value = IDL.Rec();
Icrc3Value.fill(
  IDL.Variant({
    Nat: IDL.Nat,
    Int: IDL.Int,
    Blob: IDL.Vec(IDL.Nat8),
    Text: IDL.Text,
    Array: IDL.Vec(Icrc3Value),
    Map: IDL.Vec(IDL.Tuple(IDL.Text, Icrc3Value)),
  }),
);

/** Decode a Candid-encoded ICRC-3 Value blob into its structured form. */
export function decodeIcrc3Value(blob: Uint8Array): Icrc3Value {
  return IDL.decode([Icrc3Value], blob)[0] as unknown as Icrc3Value;
}

function toHex(bytes: number[]): string {
  let s = "";
  for (const b of bytes) s += b.toString(16).padStart(2, "0");
  return s;
}

function renderValue(v: Icrc3Value): string {
  if ("Text" in v) return JSON.stringify(v.Text);
  if ("Nat" in v) return v.Nat.toString();
  if ("Int" in v) return v.Int.toString();
  if ("Blob" in v) return `0x${toHex(v.Blob)}`;
  if ("Array" in v) return `[${v.Array.map(renderValue).join(", ")}]`;
  return `{${v.Map.map(([k, val]) => `${JSON.stringify(k)}: ${renderValue(val)}`).join(", ")}}`;
}

/** Format a decoded ICRC-3 attribute map as human-readable text. */
export function formatIcrc3Attributes(blob: Uint8Array): string {
  const value = decodeIcrc3Value(blob);
  if (!("Map" in value)) {
    return renderValue(value);
  }
  return [...value.Map]
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([k, v]) => `${k}: ${renderValue(v)}`)
    .join("\n");
}
