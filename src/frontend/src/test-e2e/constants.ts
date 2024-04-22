import { KnownDapp } from "$src/flows/dappsExplorer/dapps";
import { readCanisterId } from "@dfinity/internet-identity-vite-plugins/utils";

// XXX: this is not exactly a constant (since it might change on every node eval) but in
// practice is very stable, and is much easier to use as "constants" than as a lookup function.
const testAppCanisterId = readCanisterId({ canisterName: "test_app" });
const issuerAppCanisterId = readCanisterId({ canisterName: "issuer" });

export const TEST_APP_CANONICAL_URL = `https://${testAppCanisterId}.icp0.io`;
export const TEST_APP_CANONICAL_URL_RAW = `https://${testAppCanisterId}.raw.icp0.io`;
export const TEST_APP_CANONICAL_URL_LEGACY = `https://${testAppCanisterId}.ic0.app`;
export const TEST_APP_NICE_URL = "https://nice-name.com";
export const KNOWN_TEST_DAPP = new KnownDapp({
  name: "Test Dapp",
  website: "https://nice-name.com",
  logo: "no-such-logo",
});

export const ISSUER_APP_URL = `https://${issuerAppCanisterId}.icp0.io`;
export const ISSUER_APP_URL_LEGACY = `https://${issuerAppCanisterId}.ic0.app`;

// Value needs to match how the canister was provisioned
export const ISSUER_CUSTOM_ORIGIN_NICE_URL = `https://nice-issuer-custom-orig.com`;

export const II_URL =
  process.env.II_URL ?? "https://identity.internetcomputer.org";

export const DEVICE_NAME1 = "FIDO Passkey";
export const RECOVERY_PHRASE_NAME = "Recovery Phrase";

// Some user agents

// Chrome on macOS
export const APPLE_USER_AGENT =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36";

// Edge on Windows
export const EDGE_USER_AGENT =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36 Edg/116.0.1938.81";
