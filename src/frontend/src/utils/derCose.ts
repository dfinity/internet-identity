const encodeLenBytes = (len: number): number => {
  if (len <= 0x7f) {
    return 1;
  } else if (len <= 0xff) {
    return 2;
  } else if (len <= 0xffff) {
    return 3;
  } else if (len <= 0xffffff) {
    return 4;
  } else {
    throw new Error("Length too long (> 4 bytes)");
  }
};

const writeLenBytes = (
  buf: Uint8Array,
  offset: number,
  len: number
): number => {
  if (len <= 0x7f) {
    buf[offset] = len;
    return 1;
  } else if (len <= 0xff) {
    buf[offset] = 0x81;
    buf[offset + 1] = len;
    return 2;
  } else if (len <= 0xffff) {
    buf[offset] = 0x82;
    buf[offset + 1] = len >> 8;
    buf[offset + 2] = len;
    return 3;
  } else if (len <= 0xffffff) {
    buf[offset] = 0x83;
    buf[offset + 1] = len >> 16;
    buf[offset + 2] = len >> 8;
    buf[offset + 3] = len;
    return 4;
  } else {
    throw new Error("Length too long (> 4 bytes)");
  }
};

const readLenBytes = (buf: Uint8Array, offset: number): number => {
  if (buf[offset] < 0x80) return 1;
  if (buf[offset] === 0x80) throw new Error("Invalid length 0");
  if (buf[offset] === 0x81) return 2;
  if (buf[offset] === 0x82) return 3;
  if (buf[offset] === 0x83) return 4;
  throw new Error("Length too long (> 4 bytes)");
};

const oid = new Uint8Array([
  0x30,
  0x0c,
  0x06,
  0x0a,
  0x2b,
  0x06,
  0x01,
  0x04,
  0x01,
  0x83,
  0xb8,
  0x43,
  0x01,
  0x01,
]);

export const wrapDERCose = (cose: ArrayBuffer): ArrayBuffer => {
  // The Bit String header needs to include the unused bit count byte in its length
  const bitStringHeader = 2 + encodeLenBytes(cose.byteLength + 1);
  const len = oid.byteLength + bitStringHeader + cose.byteLength;
  let offset = 0;
  const buf = new Uint8Array(1 + encodeLenBytes(len) + len);
  // Sequence
  buf[offset++] = 0x30;
  // Sequence Length
  offset += writeLenBytes(buf, offset, len);

  // OID
  buf.set(oid, offset);
  offset += oid.byteLength;

  // Bit String Header
  buf[offset++] = 0x03;
  offset += writeLenBytes(buf, offset, cose.byteLength + 1);
  // 0 padding
  buf[offset++] = 0x00;
  buf.set(new Uint8Array(cose), offset);

  return buf;
};

export const unwrapDERCose = (derEncoded: ArrayBuffer): ArrayBuffer => {
  let offset = 0;
  const expect = (n: number, msg: string) => {
    if (buf[offset++] !== n) throw new Error("Expected: " + msg);
  };
  const buf = new Uint8Array(derEncoded);
  expect(0x30, "sequence");
  offset += readLenBytes(buf, offset);
  offset += oid.byteLength;
  expect(0x03, "bit string");
  offset += readLenBytes(buf, offset);
  expect(0x00, "0 padding");
  return buf.slice(offset);
};

// Utilities

export const toHex = (buf: ArrayBuffer): string => {
  let res = "";
  new Uint8Array(buf).forEach((byte) => {
    const s = byte.toString(16);
    res += s.length === 1 ? "0" + s : s;
  });
  return res;
};

export const blobEquals = (b1: ArrayBuffer, b2: ArrayBuffer): boolean => {
  if (b1.byteLength !== b2.byteLength) return false;
  const u1 = new Uint8Array(b1);
  const u2 = new Uint8Array(b2);
  for (let i = 0; i < u1.length; i++) {
    if (u1[i] !== u2[i]) return false;
  }
  return true;
};
