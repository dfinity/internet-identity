// A `hasOwnProperty` that produces evidence for the typechecker
export function hasOwnProperty<
  X extends Record<string, unknown>,
  Y extends PropertyKey
>(obj: X, prop: Y): obj is X & Record<Y, unknown> {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

export function bufferEquals(b1: ArrayBuffer, b2: ArrayBuffer): boolean {
  if (b1.byteLength !== b2.byteLength) return false;
  let u1 = new Uint8Array(b1);
  let u2 = new Uint8Array(b2);
  for (let i = 0; i < b1.byteLength; i++) {
    if (u1[i] !== u2[i]) return false;
  }
  return true;
}
