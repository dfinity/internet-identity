// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { readFileSync } from "fs";
export const test_app_canister_ids = JSON.parse(
  readFileSync("./demos/test-app/.dfx/local/canister_ids.json", "utf-8")
);

const TEST_APP_CANISTER_ID = test_app_canister_ids.test_app.local;

export const TEST_APP_CANONICAL_URL = `https://${TEST_APP_CANISTER_ID}.icp0.io`;
export const TEST_APP_CANONICAL_URL_RAW = `https://${TEST_APP_CANISTER_ID}.raw.icp0.io`;
export const TEST_APP_CANONICAL_URL_LEGACY = `https://${TEST_APP_CANISTER_ID}.ic0.app`;
export const TEST_APP_NICE_URL = "https://nice-name.com";
export const II_URL =
  process.env.II_URL ?? "https://identity.internetcomputer.org";

export const DEVICE_NAME1 = "FIDO Passkey";
export const RECOVERY_PHRASE_NAME = "Recovery Phrase";
