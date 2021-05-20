import { blobFromUint32Array } from '@dfinity/agent';
import { entropyToMnemonic } from 'bip39';

/**
 * @returns A random BIP39 mnemonic with 256 bits of entropy.
 */
export function generateMnemonic(): string {
  const entropy = new Uint32Array(32);
  crypto.getRandomValues(entropy);
  return entropyToMnemonic(blobFromUint32Array(entropy).toString('hex'));
}
