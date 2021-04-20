import { WebAuthnIdentity } from "@dfinity/identity";
import { MultiWebAuthnIdentity } from "./multiWebauthnIdentity";

type AuthenticationOptions = {};

export const authenticateFresh = () => {
  return WebAuthnIdentity.create().then((identity: WebAuthnIdentity) => {
    persistIdentity(identity);
    return identity;
  });
}

export const authenticate = (opts?: AuthenticationOptions) => {
  const stored_identity = tryLoadIdentity()
  if (stored_identity === null || stored_identity === undefined) {
    return authenticateFresh();
  } else {
    return stored_identity;
  }
};

export const tryLoadIdentity = () => {
  const stored_identity = localStorage.getItem("identity");
  return stored_identity ? WebAuthnIdentity.fromJSON(stored_identity) : undefined
};

export const persistIdentity = (identity: WebAuthnIdentity) => {
  localStorage.setItem("identity", JSON.stringify(identity.toJSON()));
};

export default {};
