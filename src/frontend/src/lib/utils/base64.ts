/**
 * Decodes a base64 encoded string
 * @param str to decode
 * @returns decoded string
 */
export const decodeBase64 = (str: string): string =>
  // We can use of atob() because the only strings we are decoding are SVG logos, which are safe
  atob(str);
