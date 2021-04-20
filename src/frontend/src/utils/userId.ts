export const getUserId = () : bigint | undefined => {
  const userId = localStorage.getItem("userId");
  return userId ? BigInt(userId) : undefined;
}

export const setUserId = (userId: bigint | undefined) => {
  if (userId !== undefined) {
    localStorage.setItem("userId", userId.toString());
  } else {
    localStorage.removeItem("userId");
  }
}
