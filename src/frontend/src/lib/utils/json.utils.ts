import { Principal } from "@dfinity/principal";
import { nonNullish } from "@dfinity/utils";
import { fromBase64, toBase64 } from "./utils";

const JSON_KEY_BIGINT = "__bigint__";
const JSON_KEY_PRINCIPAL = "__principal__";
const JSON_KEY_UINT8ARRAY = "__uint8array__";
const JSON_KEY_ARRAYBUFFER = "__arraybuffer__";

/**
 * A custom replacer for `JSON.stringify` that converts specific types not natively supported
 * by the API into JSON-compatible formats.
 *
 * Supported conversions:
 * - `BigInt` → `{ "__bigint__": string }`
 * - `Principal` → `{ "__principal__": string }`
 * - `Uint8Array` → `{ "__uint8array__": number[] }`
 * - `ArrayBuffer` → `{ "__arraybuffer__": string }` (base64 encoded)
 *
 * @param {string} _key - Ignored. Only provided for API compatibility.
 * @param {unknown} value - The value to transform before stringification.
 * @returns {unknown} The transformed value if it matches a known type, otherwise the original value.
 */
export const jsonReplacer = (_key: string, value: unknown): unknown => {
  if (typeof value === "bigint") {
    return { [JSON_KEY_BIGINT]: `${value}` };
  }

  if (nonNullish(value) && value instanceof Principal) {
    return { [JSON_KEY_PRINCIPAL]: value.toText() };
  }

  if (nonNullish(value) && value instanceof Uint8Array) {
    return { [JSON_KEY_UINT8ARRAY]: Array.from(value) };
  }

  if (nonNullish(value) && value instanceof ArrayBuffer) {
    return { [JSON_KEY_ARRAYBUFFER]: toBase64(value) };
  }

  return value;
};

/**
 * A custom reviver for `JSON.parse` that reconstructs specific types from their JSON-encoded representations.
 *
 * This reverses the transformations applied by `jsonReplacer`, restoring the original types.
 *
 * Supported conversions:
 * - `{ "__bigint__": string }` → `BigInt`
 * - `{ "__principal__": string }` → `Principal`
 * - `{ "__uint8array__": number[] }` → `Uint8Array`
 * - `{ "__arraybuffer__": string }` → `ArrayBuffer` (from base64)
 *
 * @param {string} _key - Ignored but provided for API compatibility.
 * @param {unknown} value - The parsed value to transform.
 * @returns {unknown} The reconstructed value if it matches a known type, otherwise the original value.
 */
export const jsonReviver = (_key: string, value: unknown): unknown => {
  const mapValue = <T>(key: string): T => (value as Record<string, T>)[key];

  if (
    nonNullish(value) &&
    typeof value === "object" &&
    JSON_KEY_BIGINT in value
  ) {
    return BigInt(mapValue(JSON_KEY_BIGINT));
  }

  if (
    nonNullish(value) &&
    typeof value === "object" &&
    JSON_KEY_PRINCIPAL in value
  ) {
    return Principal.fromText(mapValue(JSON_KEY_PRINCIPAL));
  }

  if (
    nonNullish(value) &&
    typeof value === "object" &&
    JSON_KEY_UINT8ARRAY in value
  ) {
    return Uint8Array.from(mapValue(JSON_KEY_UINT8ARRAY));
  }

  if (
    nonNullish(value) &&
    typeof value === "object" &&
    JSON_KEY_ARRAYBUFFER in value
  ) {
    return fromBase64(mapValue(JSON_KEY_ARRAYBUFFER));
  }

  return value;
};
