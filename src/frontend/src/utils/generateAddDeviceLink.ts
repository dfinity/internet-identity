import { WebAuthnIdentity } from "@dfinity/identity";
import { blobToHex, DerEncodedBlob } from "@dfinity/agent";
import { persistIdentity } from "./handleAuthentication";
import idp_actor from "./idp_actor";

// Generates a link that contains the User Id, the DER encoded PublicKey as well as the CredentialId
// Also returns the `publicKey` of the used device, to let us poll whether this device was accepted
export const generateAddDeviceLink = async (userId: bigint): Promise<{ link: string, publicKey: DerEncodedBlob }> => {
  if (!idp_actor.storedIdentity) {
    idp_actor.storedIdentity = await WebAuthnIdentity.create();
    persistIdentity(idp_actor.storedIdentity);
  }
  const publicKey = idp_actor.storedIdentity.getPublicKey().toDer();
  const rawId = blobToHex(idp_actor.storedIdentity.rawId);

  // TODO: Maybe we should add a checksum here, to make sure the user didn't copy a cropped link
  return {
    link: encodeURI(
      `${location.host}/manage#device=${userId};${blobToHex(publicKey)};${rawId}`
    ),
    publicKey: publicKey
  };
};
