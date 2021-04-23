// run this with
// npx ts-node pow.ts 000400000000000001 0
// (parameters are canister id and timestamp)

import cubeHash from "./src/frontend/src/crypto/cubehash"
import { Principal } from "@dfinity/agent";
import bigUintLE from "biguintle";

const IDP_CANISTER_ID: string = process.env.CANISTER_ID!;
const DIFFICULTY = 2; // Number of leading bytes that must equal zero in the hash.

const DOMAIN = "ic-proof-of-work";
const CANISTER_ID_BLOB = Buffer.from(process.argv[2], "hex");
const NONCE_OFFSET = DOMAIN.length + 1 /* domain + prefix */ + 8 /* timestamp length */;

function toLeBytes(num: BigInt): Buffer {
  const b = Buffer.alloc(8);
  bigUintLE.encode(num, b);
  return b;
}

// Returns true if the hash passes the set level of difficulty.
function hashOk(hash: Uint8Array): boolean {
  for (let i = 0; i < DIFFICULTY; i++) {
    if (hash[i] != 0) {
      return false;
    }
  }
  return true;
}

const timestamp = BigInt(process.argv[3]);
let nonce = BigInt(0);

let message = Buffer.concat([
    Buffer.from([DOMAIN.length]),
    Buffer.from(DOMAIN),
    toLeBytes(timestamp),
    toLeBytes(nonce),
    Buffer.from([CANISTER_ID_BLOB.length]),
    CANISTER_ID_BLOB
]);

console.log("Canister id:", CANISTER_ID_BLOB);
console.log("Timestamp:", timestamp);

// Keep incrementing the nonce until we find a hash that checks.
while (true) {
  const hash = cubeHash(message);
  if (hashOk(hash)) {
    break;
  }

  // Hash doesn't check. Increment nonce and update the message.
  nonce += BigInt(1);
  let nonce_encoded = toLeBytes(nonce);
  for (let i = 0; i < nonce_encoded.length; i++) {
    message[NONCE_OFFSET + i] = nonce_encoded[i];
  }
}

console.log("Nonce:", nonce);
