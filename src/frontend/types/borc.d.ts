/* eslint-disable @typescript-eslint/no-explicit-any */
/**
 * Copied from agent-js package along WebAuthnIdentity.
 *
 * This can be removed once we import `WebAuthnIdentity` from agent-js again.
 */
declare module "borc" {
  class Decoder {
    constructor(opts: {
      size: number;
      tags: Record<number, (val: any) => any>;
    });

    decodeFirst(input: ArrayBuffer): any;
  }

  export function decodeFirst(input: ArrayBuffer): any;

  export function encode(o: any): Uint8Array;

  class Tagged {
    tag: number;
    value: any;
    constructor(tag: number, value: any);
  }
}
