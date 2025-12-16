import knownPasswordManagers from "./password_managers.json";
import knownSecurityKeys from "./security_keys.json";

/**
 * e.g. Stored in your Google account and synced across your Android devices.
 */
interface StoredInAccount {
  type: "account";
  account: string;
  platform?: string;
}

/**
 * e.g. Stored and usable only on the Windows device it was created on.
 */
interface StoredOnPlatform {
  type: "platform";
  platform: string;
}

/**
 * e.g. Kept on a physical key. Authenticate on supported devices via tap/insert.
 */
interface StoredOnDevice {
  type: "device";
}

/**
 * e.g. Stored and usable only in Chrome on the macOS device it was created on.`
 */
interface StoredInBrowser {
  type: "browser";
  browser: string;
  platform?: string;
}

export type Provider = { name: string } & (
  | StoredInAccount
  | StoredOnPlatform
  | StoredOnDevice
  | StoredInBrowser
);

// Passkey providers are listed from least to most common (top to bottom),
// because in JavaScript, properties defined later override earlier ones.
const knownProviders: Record<string, Provider> = {
  // Password managers (e.g. Bitwarden browser extension)
  ...Object.fromEntries(
    Object.entries(knownPasswordManagers).map(([aaguid, name]) => [
      aaguid,
      {
        name,
        type: "account",
        account: name,
      },
    ]),
  ),
  // Security keys (e.g. YubiKey)
  ...Object.fromEntries(
    Object.entries(knownSecurityKeys).map(([aaguid, name]) => [
      aaguid,
      {
        name,
        type: "device",
      },
    ]),
  ),
  // Browsers (when iCloud is not enabled on Mac)
  "adce0002-35bc-c60a-648b-0b25f1f05503": {
    name: "Chrome",
    type: "browser",
    browser: "Chrome",
    platform: "macOS",
  },
  "771b48fd-d3d4-4f74-9232-fc157ab0507a": {
    name: "Edge",
    type: "browser",
    browser: "Edge",
    platform: "macOS",
  },
  // Operating systems (provider available by default)
  ...Object.fromEntries(
    [
      "08987058-cadc-4b81-b6e1-30de50dcbe96",
      "9ddd1817-af5a-4672-a2b9-3e3dd95000a9",
      "6028b017-b1d4-4c02-b4b3-afcdafc96bb2",
    ].map((aaguid) => [
      aaguid,
      {
        name: "Windows Hello",
        type: "platform",
        platform: "Windows",
      },
    ]),
  ),
  ...Object.fromEntries(
    [
      "dd4ec289-e01d-41c9-bb89-70fa845d4bf2",
      "fbfc3007-154e-4ecc-8c0b-6e020557d7bd",
    ].map((aaguid) => [
      aaguid,
      {
        name: "Apple Passwords",
        type: "account",
        account: "Apple",
        platform: "Apple",
      },
    ]),
  ),
  "ea9b8d66-4d01-1d21-3ce4-b6b48cb575d4": {
    name: "Google Password Manager",
    type: "account",
    account: "Google",
    platform: "Android",
  },
  "53414d53-554e-4700-0000-000000000000": {
    name: "Samsung Pass",
    type: "account",
    account: "Samsung",
    platform: "Samsung",
  },
};

export default knownProviders;
