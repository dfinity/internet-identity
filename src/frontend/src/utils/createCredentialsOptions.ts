enum PubKeyCoseAlgo {
  ECDSA_WITH_SHA256 = -7,
}

// used for credential.create, only per-user uniqueness is important
function randomUser() {
  let result = "";
  for (let i = 0; i < 16; i++) {
    result += [Math.floor(Math.random() * 10)];
  }
  return result;
}

type Options = {
  challenge?: string;
  name?: string;
  id?: bigint;
};
export const createCredentialsOptions = (
  options?: Options
): CredentialCreationOptions => {
  const challenge = options?.challenge || "identity.ic0.app";
  const name = options?.name || "Internet Identity";
  const id = options?.id ? options?.id.toString() : randomUser();
  return {
    publicKey: {
      authenticatorSelection: {
        userVerification: "preferred",
      },
      attestation: "direct",
      challenge: Uint8Array.from(challenge, (c) => c.charCodeAt(0)),
      pubKeyCredParams: [
        { type: "public-key", alg: PubKeyCoseAlgo.ECDSA_WITH_SHA256 },
      ],
      rp: {
        name: "Internet Identity Service",
      },
      user: {
        id: Buffer.from(id),
        name,
        displayName: name,
      },
    },
  };
};
