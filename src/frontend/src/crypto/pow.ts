import CubeHash from "cubehash";
import { Principal } from "@dfinity/agent";
import bigUintLE from "biguintle";
import { ProofOfWork } from "../../generated/idp_types";

const IDP_CANISTER_ID: string = process.env.CANISTER_ID!;
const DIFFICULTY = 2; // Number of leading bytes that must equal zero in the hash.
const DOMAIN = "ic-proof-of-work";
const CANISTER_ID_BLOB = Principal.from(IDP_CANISTER_ID).toBlob();
const NONCE_OFFSET = DOMAIN.length + 1 /* domain + prefix */ + 8 /* timestamp length */;

export default function(): ProofOfWork {
  console.time('PoW');
  const timestamp = BigInt(Date.now()) * BigInt(1000); // Timestamp in ns.
  let nonce = BigInt(0);

  let message = Buffer.concat([
      new Buffer([DOMAIN.length]),
      Buffer.from(DOMAIN),
      toLeBytes(timestamp),
      toLeBytes(nonce),
      new Buffer([CANISTER_ID_BLOB.length]),
      CANISTER_ID_BLOB
  ]);

  // Keep incrementing the nonce until we find a hash that checks.
  while (true) {
    const hash = CubeHash(512, message);
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
  
  console.timeEnd('PoW');
  console.log(`Nonce found: ${nonce}`);
  return {
    timestamp: timestamp,
    nonce: nonce
  }
}

function toLeBytes(num: BigInt): Buffer {
  const b = Buffer.alloc(8);
  bigUintLE.encode(num, b);
  return b;
}

// Returns true if the hash passes the set level of difficulty.
function hashOk(hash: Buffer): boolean {
  for (let i = 0; i < DIFFICULTY; i++) {
    if (hash[i] != 0) {
      return false;
    }
  }
  return true;
}
