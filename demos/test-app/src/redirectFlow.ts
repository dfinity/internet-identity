/**
 * Shared, transport-agnostic codec for the redirect sign-in flow.
 *
 * The homepage serialises its inputs into the `/callback.html` URL's query;
 * the callback page runs the ICRC-167 redirect flow and serialises the results
 * back into the homepage URL's hash. Keeping the encode/decode here (rather
 * than in either page) means the callback page stays a generic driver — it
 * never hardcodes which fields exist.
 */

/** The dedicated callback route (a separate Vite entry / canister asset). */
export const CALLBACK_PATH = "/callback.html";

/** Inputs the homepage collects and hands to the callback page. */
export interface RedirectInputs {
  /** Identity provider (II) URL. */
  iiUrl: string;
  /** Optional derivation origin. */
  derivationOrigin?: string;
  /** Max delegation lifetime in nanoseconds, as a decimal string. */
  maxTimeToLive?: string;
  /** Whether to also request ICRC-3 attributes alongside sign-in. */
  requestAttributes: boolean;
  /** Attribute keys to request when `requestAttributes` is set. */
  attributeKeys: string[];
  /** Optional base64 ICRC-3 nonce; a random one is used when absent. */
  nonce?: string;
}

export const encodeInputs = (inputs: RedirectInputs): string => {
  const params = new URLSearchParams();
  params.set("iiUrl", inputs.iiUrl);
  params.set("requestAttributes", String(inputs.requestAttributes));
  params.set("attributeKeys", JSON.stringify(inputs.attributeKeys));
  if (inputs.derivationOrigin !== undefined) {
    params.set("derivationOrigin", inputs.derivationOrigin);
  }
  if (inputs.maxTimeToLive !== undefined) {
    params.set("maxTimeToLive", inputs.maxTimeToLive);
  }
  if (inputs.nonce !== undefined) {
    params.set("nonce", inputs.nonce);
  }
  return params.toString();
};

export const decodeInputs = (search: string): RedirectInputs => {
  const params = new URLSearchParams(search);
  const attributeKeys = params.get("attributeKeys");
  return {
    iiUrl: params.get("iiUrl") ?? "",
    derivationOrigin: params.get("derivationOrigin") ?? undefined,
    maxTimeToLive: params.get("maxTimeToLive") ?? undefined,
    requestAttributes: params.get("requestAttributes") === "true",
    attributeKeys:
      attributeKeys !== null ? (JSON.parse(attributeKeys) as string[]) : [],
    nonce: params.get("nonce") ?? undefined,
  };
};

/** Results the callback page hands back to the homepage. The delegation itself
 *  isn't carried here — the homepage recovers it from `AuthClient`'s persisted
 *  session (as any redirect RP would). Only one-shot values ride the URL. */
export interface RedirectResults {
  /** Base64 ICRC-3 attribute data + signature, when attributes were requested. */
  attributes?: { data: string; signature: string };
  /** Error message when the flow failed. */
  error?: string;
}

/** Marks a homepage load as the return leg of a redirect flow. */
const RESULT_MARKER = "redirectResult";

export const encodeResults = (results: RedirectResults): string => {
  const params = new URLSearchParams();
  params.set(RESULT_MARKER, "1");
  if (results.error !== undefined) {
    params.set("error", results.error);
  }
  if (results.attributes !== undefined) {
    params.set("attributes", JSON.stringify(results.attributes));
  }
  return params.toString();
};

/** Decodes results from a homepage load's hash, or `undefined` when the load
 *  is not a redirect return. */
export const decodeResults = (hash: string): RedirectResults | undefined => {
  const params = new URLSearchParams(hash.replace(/^#/, ""));
  if (params.get(RESULT_MARKER) !== "1") {
    return undefined;
  }
  const attributes = params.get("attributes");
  return {
    error: params.get("error") ?? undefined,
    attributes:
      attributes !== null
        ? (JSON.parse(attributes) as RedirectResults["attributes"])
        : undefined,
  };
};
