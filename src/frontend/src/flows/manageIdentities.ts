import { BinaryBlob, blobFromHex, derBlobFromBlob, DerEncodedBlob } from "@dfinity/agent";

export const initManageIdentities = () => {
  // TODO - Check alias for current identity, and populate #nameSpan
  // TODO - Lookup identities then render them based on the result.
  //   TODO - check if authenticated, handle login if needed
  // Check URL if user has pasted in an Add Identity link
  const url = new URL(document.URL);
  const newDevice = url.searchParams.get("device");
  if (newDevice !== null) {
    const parsedParams = parseNewDeviceParam(newDevice);
    if (parsedParams !== null) {
      const { publicKey, rawId } = parsedParams;
      console.log("Adding new device with:", parsedParams)
      // TODO: Prompt the user for an alias and let them add the new device to their
      // existing identity
    }
  }
};

const parseNewDeviceParam = (param: string): { publicKey: DerEncodedBlob, rawId: BinaryBlob } | null => {
  const segments = param.split(";");
  if (segments.length !== 2) {
    // TODO: Decent error handling
    console.error("This is not a valid pasted link");
    return null
  }
  const publicKey = derBlobFromBlob(blobFromHex(segments[0]));
  const rawId = blobFromHex(segments[1]);
  return { publicKey, rawId }
}
