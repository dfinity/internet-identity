import { vi } from "vitest";

// Allow agent-js canister call within tests to self-signed certificate URL
if (process.env.NODE_ENV === "test") {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}

// We mock the environment variable because jest is not able to load import.meta.env
vi.mock("./src/environment.ts", () => ({
  BASE_URL: "/",
  FETCH_ROOT_KEY: false,
  DUMMY_AUTH: false,
  DUMMY_CAPTCHA: false,
  VERSION: ",,clean",
}));

export type WebAuthnCredential = {
  credentialId: string;
  isResidentCredential: boolean;
  privateKey: string;
  signCount: number;
  rpId?: string;
  userHandle?: string;
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
        credentialId: string,
        isResidentCredential: boolean,
        rpId: string,
        privateKey: string,
        signCount: number,
        userHandle?: string,
        largeBlob?: string
      ) => Promise<void>;
    }
  }
}
