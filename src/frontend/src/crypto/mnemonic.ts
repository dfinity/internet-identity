import { toHexString } from "@dfinity/candid/lib/cjs/utils/buffer";
import { entropyToMnemonic, wordlists, validateMnemonic } from "bip39";

/**
 * @returns A random BIP39 mnemonic with 256 bits of entropy.
 */
export function generate(): string {
  const entropy = new Uint32Array(32);
  crypto.getRandomValues(entropy);
  return entropyToMnemonic(toHexString(entropy), wordlists.english);
}

/**
 * @returns true if the mnemonic is valid, false otherwise.
 */
export function validate(mnemonic: string): boolean {
  return validateMnemonic(mnemonic);
}
