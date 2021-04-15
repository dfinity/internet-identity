import { BinaryBlob, blobFromHex, derBlobFromBlob, DerEncodedBlob } from "@dfinity/agent";
import idp_actor from "../utils/idp_actor";

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
      const { userId, publicKey, rawId } = parsedParams;

      // TODO: Prompt the user for an alias and let them add the new device to their
      // existing identity
      const alias = window.prompt("Gief alias:");
      console.log("Adding new device with:", parsedParams)
      idp_actor.add(userId, alias!!, publicKey, rawId)
    }
  }
};

const parseNewDeviceParam = (param: string): { userId: bigint, publicKey: DerEncodedBlob, rawId?: BinaryBlob } | null => {
  const segments = param.split(";");
  if (!(segments.length === 2 || segments.length === 3)) {
    // TODO: Decent error handling
    console.error("This is not a valid pasted link");
    return null
  }
  const userId = BigInt(segments[0]);
  const publicKey = derBlobFromBlob(blobFromHex(segments[1]));
  const rawId = segments[2] ? blobFromHex(segments[2]) : undefined;
  return { userId, publicKey, rawId }
}
