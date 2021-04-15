import { WebAuthnIdentity } from "@dfinity/identity";

type AuthenticationOptions = {};

export const authenticate = (opts?: AuthenticationOptions) => {
  WebAuthnIdentity.create; //?
  return WebAuthnIdentity.create().then((identity: WebAuthnIdentity) => {
    persistIdentity(identity);
    return identity;
  });
};

export const persistIdentity = (identity: WebAuthnIdentity) => {
  localStorage.setItem("identity", JSON.stringify(identity.toJSON()));
};

export default {};
