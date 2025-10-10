import { sha256 } from "js-sha256";
// noinspection ES6PreferShortImport
import { toBase64 } from "../frontend/src/lib/utils/utils";

const UNIT_SEPARATOR = "\u001F";

// Generates a short, deterministic ID by hashing (message + context),
// converting the SHA-256 hex digest to bytes, encoding as Base64, and
// taking the first 6 chars for a compact identifier.
export const generateMessageId = (message: string, context = "") =>
  toBase64(
    new Uint8Array(
      sha256(message + UNIT_SEPARATOR + context)
        .match(/.{1,2}/g)!
        .map((byte) => parseInt(byte, 16)),
    ),
  ).slice(0, 6);
