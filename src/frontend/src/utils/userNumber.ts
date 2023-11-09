// We check that the user has entered a sequence of digits only,
// before attempting to parse
export const parseUserNumber = (s: string): bigint | null => {
  if (isUserNumber(s)) {
    try {
      return BigInt(s);
    } catch (err) {
      return null;
    }
  } else {
    return null;
  }
};

// This makes sure `s` is a viable user number; naively using BigInt
// on any string would also accept the following values, which are
// _not_ valid user numbers:
// - BigInt(whitespace) == 0
// - Hex/Octal formatted numbers
// - Scientific notation
export const isUserNumber = (s: string): boolean => {
  return /^\d+$/.test(s);
};
