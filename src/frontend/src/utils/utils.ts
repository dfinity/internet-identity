// A `hasOwnProperty` that produces evidence for the typechecker
export function hasOwnProperty<X extends Record<string, unknown>, Y extends PropertyKey>
  (obj: X, prop: Y): obj is X & Record<Y, unknown> {
  return Object.prototype.hasOwnProperty.call(obj, prop)
}