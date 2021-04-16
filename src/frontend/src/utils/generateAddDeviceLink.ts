import { WebAuthnIdentity } from "@dfinity/identity";
import { blobToHex } from "@dfinity/agent";
import { persistIdentity } from "./handleAuthentication";
import idp_actor from "./idp_actor";

// Generates a link that contains the User Id, the DER encoded PublicKey as well as the CredentialId

export const generateAddDeviceLink = async (userId: bigint) => {
  if (!idp_actor.storedIdentity) {
    idp_actor.storedIdentity = await WebAuthnIdentity.create();
    persistIdentity(idp_actor.storedIdentity);
  }
  const publicKey = blobToHex(idp_actor.storedIdentity.getPublicKey().toDer());
  const rawId = blobToHex(idp_actor.storedIdentity.rawId);

  // TODO: Maybe we should add a checksum here, to make sure the user didn't copy a cropped link
  // const url = `https://auth0.ic.app/manage.html?device=${publicKey};${rawId}`;
  return encodeURI(
    `localhost:8080/manage.html#device=${userId};${publicKey};${rawId}`
  );
};
