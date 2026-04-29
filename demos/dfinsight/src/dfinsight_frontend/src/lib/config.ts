/// <reference types="vite/client" />

export const BACKEND_CANISTER_ID =
  (import.meta.env.VITE_DFINSIGHT_BACKEND_CANISTER_ID as string | undefined) ||
  // Local replica default — overridden once the canister id is wired
  // through `vite.config.ts` from `CANISTER_ID_DFINSIGHT_BACKEND`.
  "uxrrr-q7777-77774-qaaaq-cai";

export const IC_HOST =
  (import.meta.env.VITE_IC_HOST as string | undefined) ||
  "http://127.0.0.1:4943";

// Where users get redirected for SSO. The `?sso=dfinity.org` flag puts
// id.ai into the 1-click discovery flow described in the II spec — for
// signed-in DFINITY members this should be a single tap.
export const II_URL =
  (import.meta.env.VITE_II_URL as string | undefined) ||
  "https://id.ai/authorize?sso=dfinity.org";

// II's production canister principal — every signed attribute bundle
// is signed by this principal, and the backend's
// `trusted_attribute_signers` env var must match.
export const II_CANISTER_ID = "rdmx6-jaaaa-aaaaa-aaadq-cai";

export const isLocal =
  IC_HOST.includes("127.0.0.1") || IC_HOST.includes("localhost");
