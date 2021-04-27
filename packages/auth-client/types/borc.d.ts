declare module 'borc' {
  import { Buffer } from 'buffer/';

  class Decoder {
    constructor(opts: { size: number; tags: Record<number, (val: any) => any> });

    decodeFirst(input: ArrayBuffer): any;
  }

  export function decodeFirst(input: ArrayBuffer): any;

  export function encode(o: any): Buffer;

  class Tagged {
    tag: number;
    value: any;
    constructor(tag: Number, value: any);
  }
}
