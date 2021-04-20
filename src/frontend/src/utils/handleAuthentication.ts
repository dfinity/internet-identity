import { WebAuthnIdentity } from "@dfinity/identity";

export const authenticateFresh = () => {
  return WebAuthnIdentity.create().then((identity: WebAuthnIdentity) => {
    return identity;
  });
}

export default {};
