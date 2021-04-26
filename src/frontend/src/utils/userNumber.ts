export const getUserNumber = () : bigint | undefined => {
  const userNumber = localStorage.getItem("userNumber");
  return userNumber ? BigInt(userNumber) : undefined;
}

export const setUserNumber = (userNumber: bigint | undefined) => {
  if (userNumber !== undefined) {
    localStorage.setItem("userNumber", userNumber.toString());
  } else {
    localStorage.removeItem("userNumber");
  }
}
