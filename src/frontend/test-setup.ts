import crypto from "@trust/webcrypto";
import { TextEncoder } from "util";
import { vi } from "vitest";

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
        rpId: string,
        credentialId: string,
        isResidentCredential: boolean,
        privateKey: string,
        signCount: number,
        userHandle?: string,
        largeBlob?: string
      ) => Promise<void>;
    }
  }
}

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
global.crypto.subtle = crypto.subtle;
global.TextEncoder = TextEncoder;
