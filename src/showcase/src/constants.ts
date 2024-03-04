import { DeviceData } from "$generated/internet_identity_types";
import { loadIdentityBackground } from "$src/components/identityCard";
import { getDapps } from "$src/flows/dappsExplorer/dapps";

export const dapps = getDapps();

export const iiLegacyOrigin = "https://identity.ic0.app";

const recoveryPhraseText =
  "10050 mandate vague same suspect eight pet gentle repeat maple actor about legal sword text food print material churn perfect sword blossom sleep vintage blouse";

export const recoveryAnchorWord = recoveryPhraseText.split(" ")[0];
export const recoveryWords = recoveryPhraseText.split(" ").slice(1);

export const userNumber = BigInt(10000);

export const chromeDevice: DeviceData = {
  alias: "Chrome on iPhone",
  protection: { unprotected: null },
  pubkey: [1, 2, 3, 4],
  key_type: { unknown: null },
  purpose: { authentication: null },
  credential_id: [],
  origin: [],
  metadata: [],
};

export const identityBackground = loadIdentityBackground();
