import { WebAuthnIdentity } from "@dfinity/identity";
import { blobToHex, blobFromHex } from "@dfinity/agent";

function setupGenerateIdentityForm() {
    const form = document.getElementById("generateIdentityForm") as HTMLFormElement;
    const generateButton = document.querySelector(
      "#generate-identity"
    ) as HTMLButtonElement;
    const decodeButton = form.querySelector(
        "#decode-identity"
      ) as HTMLButtonElement;
    const identityLink = form.querySelector("#identity-link") as HTMLDivElement;
  
    const generateId = (e) => {
      // Enter pending state
      e.preventDefault();

      return WebAuthnIdentity.create().then((identity: WebAuthnIdentity) => {
        const publicKey = blobToHex(identity.getPublicKey().toDer());
        const rawId = blobToHex(identity.getRawId());
        const url = `https://auth0.ic.app?newIdentity=${publicKey};${rawId}`;
        let params = new URLSearchParams(document.location.search);
        params.set("newIdentity", `${publicKey};${rawId}`);
        identityLink.innerText = url;
      });

  
      return false;
    };

    const decodeId = (e) => {
        // Enter pending state
        e.preventDefault();
  
        return WebAuthnIdentity.create().then((identity: WebAuthnIdentity) => {
          const publicKey = blobToHex(identity.getPublicKey().toDer());
          const rawId = blobToHex(identity.getRawId());
          const url = `https://auth0.ic.app?newIdentity=${publicKey};${rawId}`;
          identityLink.innerText = url;
        });
  
    
        return false;
      };
  
    generateButton.onclick = generateId;
    decodeButton.onclick = decodeId;
  }
  
  export default setupGenerateIdentityForm;
  