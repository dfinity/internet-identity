import { readCanisterId } from "../../../../utils";

// XXX: this is not exactly a constant (since it might change on every node eval) but in
// practice is very stable, and is much easier to use as "constants" than as a lookup function.
const testAppCanisterId = readCanisterId({ canisterName: "test_app" });
const issuerAppCanisterId = readCanisterId({ canisterName: "issuer" });

export const TEST_APP_CANONICAL_URL = `https://${testAppCanisterId}.icp0.io`;
export const TEST_APP_CANONICAL_URL_RAW = `https://${testAppCanisterId}.raw.icp0.io`;
export const TEST_APP_CANONICAL_URL_LEGACY = `https://${testAppCanisterId}.ic0.app`;
export const TEST_APP_NICE_URL = "https://nice-name.com";

export const ISSUER_APP_URL = `https://${issuerAppCanisterId}.icp0.io`;

export const II_URL =
  process.env.II_URL ?? "https://identity.internetcomputer.org";

export const DEVICE_NAME1 = "FIDO Passkey";
export const RECOVERY_PHRASE_NAME = "Recovery Phrase";
