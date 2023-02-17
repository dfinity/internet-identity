// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import test_app_canister_ids from "../../../../demos/test-app/.dfx/local/canister_ids.json";

export const TEST_APP_CANISTER_ID = test_app_canister_ids.test_app.local;
export const TEST_APP_CANONICAL_URL = `https://${TEST_APP_CANISTER_ID}.ic0.app`;
export const TEST_APP_NICE_URL = "https://nice-name.com";
export const REPLICA_URL = "https://icp-api.io";
export const II_URL = "https://identity.internetcomputer.org";
export const ABOUT_URL = `${II_URL}/about`;

export const DEVICE_NAME1 = "Virtual WebAuthn device";
export const DEVICE_NAME2 = "Other WebAuthn device";
