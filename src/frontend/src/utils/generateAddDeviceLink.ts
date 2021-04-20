import { WebAuthnIdentity } from "@dfinity/identity";
import { blobToHex, DerEncodedBlob } from "@dfinity/agent";

// Generates a link that contains the User Id, the DER encoded PublicKey as well as the CredentialId
// Also returns the `publicKey` of the used device, to let us poll whether this device was accepted
export const generateAddDeviceLink = async (userId: bigint): Promise<{ link: string, publicKey: DerEncodedBlob }> => {
  const identity = await WebAuthnIdentity.create()
  const publicKey = identity.getPublicKey().toDer();
  const rawId = blobToHex(identity.rawId);

  // TODO: Maybe we should add a checksum here, to make sure the user didn't copy a cropped link
  return {
    link: encodeURI(
      `${location.host}#device=${userId};${blobToHex(publicKey)};${rawId}`
    ),
    publicKey: publicKey
  };
};
