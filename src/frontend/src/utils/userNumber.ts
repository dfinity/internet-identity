export const getUserNumber = (): bigint | undefined => {
  const userNumber = localStorage.getItem("userNumber");
  return userNumber !== null ? BigInt(userNumber) : undefined;
};

export const setUserNumber = (userNumber: bigint | undefined): void => {
  if (userNumber !== undefined) {
    localStorage.setItem("userNumber", userNumber.toString());
  } else {
    localStorage.removeItem("userNumber");
  }
};

/** Whether or not the user is returning (false for first time users) */
export const returning = (): boolean => getUserNumber() !== undefined;

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
