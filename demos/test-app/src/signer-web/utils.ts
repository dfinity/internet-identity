export const urlIsSecureContext = (value: string): boolean => {
  try {
    const url = new URL(value);
    return (
      url.protocol === "https:" ||
      url.hostname === "127.0.0.1" ||
      url.hostname.split(".").slice(-1)[0] === "localhost"
    );
  } catch {
    return false;
  }
};
