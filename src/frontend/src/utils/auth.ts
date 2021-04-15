import { UserId, PublicKey } from "../typings";
import { Principal } from "@dfinity/agent";


/**
 * TODO: Add tests for the following test vectors.
~/dfinity/ic-ref/impl $ cabal repl ic-idp-test
…
Ok, 43 modules loaded.
*Main> :set -XOverloadedStrings 
*Main> putStrLn $ prettyBlob $ mkUserPk "" "0" -- this is canster id aaaaa-aa
0x3013300c060a2b0601040183b84301020303000030
*Main> putStrLn $ prettyBlob $ mkUserPk (BS.pack [1,2,3,4]) "123" -- four byte canister id. Note: seed is ascii-encoded decimal number
0x3019300c060a2b0601040183b84301020309000401020304313233
*/
export function getDerEncodedPublicKey(userId: UserId, signingCanister: Principal): PublicKey {
  var userIdEncoded: number[] = [];
  const userIdString = userId.toString();
  for (var x = 0; x < userIdString.length; x++) {
    userIdEncoded.push(userIdString.charCodeAt(x));
  }

  const signingCanisterBlob = signingCanister.toBlob();
  const bitString = new Uint8Array([
    signingCanisterBlob.length,
    ...signingCanisterBlob,
    ...userIdEncoded
  ]);

  return Array.from(new Uint8Array([
    0x30, 0x11 + bitString.byteLength,  // Sequence of length 17 + bitstring.length.
    0x30, 0x0C,  // Sequence of length 12
    // OID 1.3.6.1.4.1.56387.1.2
    0x06, 0x0A, 0x2B, 0x06, 0x01, 0x04, 0x01, 0x83, 0xB8, 0x43, 0x01, 0x02,
    0x03, 1 + bitString.byteLength, 0x00,  // BIT String of length bitstring.length.
    ...bitString,
  ]));
}

