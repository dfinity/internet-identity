import { WebAuthnIdentity } from "@dfinity/identity";
import { blobToHex, blobFromHex } from "@dfinity/agent";

// Generates a link that contains the DER encoded PublicKey as well as the CredentialId
// They are encoded as hex strings and separated by a semicolon
export const generateAddDeviceLink = () =>

  // TODO: This will always prompt the user to provide their WebAuthnIdentity, is there a
  // scenario where we'd use an identity already stored in localstorage here?

  WebAuthnIdentity.create().then((identity: WebAuthnIdentity) => {
    const publicKey = blobToHex(identity.getPublicKey().toDer());
    const rawId = blobToHex(identity.rawId);
    // TODO: Maybe we should add a checksum here, to make sure the user didn't copy a cropped link
    // const url = `https://auth0.ic.app/manage.html?device=${publicKey};${rawId}`;
    const url = `localhost:8080/manage.html?device=${publicKey};${rawId}`;
    return url;
  });
