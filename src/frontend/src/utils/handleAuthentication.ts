import { WebAuthnIdentity } from "@dfinity/identity";

type AuthenticationOptions = {};

export const authenticate = (opts?: AuthenticationOptions) => {
    const stored_identity = localStorage.getItem("identity");
    if (stored_identity === null) {
        return WebAuthnIdentity.create().then((identity: WebAuthnIdentity) => {
          persistIdentity(identity);
          return identity;
        });
    } else {
        return WebAuthnIdentity.fromJSON(stored_identity);
    }
};

export const persistIdentity = (identity: WebAuthnIdentity) => {
  localStorage.setItem("identity", JSON.stringify(identity.toJSON()));
};

export default {};
