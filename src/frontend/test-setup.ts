import crypto from "@trust/webcrypto";
import textEncoding = require("text-encoding");

export type WebAuthnCredential = {
  credentialId: string;
  isResidentCredential: boolean;
  privateKey: string;
  signCount: number;
  rpId: string | undefined;
};

declare global {
  // eslint-disable-next-line @typescript-eslint/no-namespace
  namespace WebdriverIO {
    interface Browser {
      addVirtualWebAuth: (
        protocol: string,
        transport: string,
        hasResidentKey: boolean,
        isUserConsenting: boolean
      ) => Promise<string>;
      removeVirtualWebAuth: (authenticatorId: string) => Promise<void>;
      getWebauthnCredentials: (
        authenticatorId: string
      ) => Promise<WebAuthnCredential[]>;
      addWebauthnCredential: (
        authenticatorId: string,
        credential: WebAuthnCredential
      ) => Promise<void>;
    }
  }
}

global.crypto = crypto;
global.TextEncoder = textEncoding.TextEncoder;
