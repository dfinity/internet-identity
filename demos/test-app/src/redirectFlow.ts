/**
 * Shared, transport-agnostic codec for the redirect sign-in flow.
 *
 * The homepage snapshots its whole sign-in form into the `/callback` URL's
 * query; the callback derives the flow inputs from that snapshot, runs the
 * ICRC-167 redirect flow, and serialises the results — including the snapshot,
 * echoed back — into the homepage URL's hash. Carrying the raw snapshot (rather
 * than a hand-picked set of fields) lets the homepage restore every option the
 * user set, and keeps the callback a generic driver: it reads only the few
 * fields it needs to drive the flow and passes the rest through untouched.
 */

/** The dedicated callback route. Built as the `callback.html` Vite entry, but
 *  the canister serves it at the extensionless `/callback` (asset_util strips
 *  the `.html`), so that's the path — and thus the ICRC-167 callback URL. */
export const CALLBACK_PATH = "/callback";

/** A raw snapshot of the sign-in form: control id → text value, or checked state
 *  for a checkbox/radio. The homepage decides which controls go in and how to
 *  restore them; the codec treats it as opaque except for deriving the flow
 *  inputs below. */
export type FormSnapshot = Record<string, string | boolean>;

/** The flow inputs the callback needs, derived from the form snapshot. */
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
  /** Same-origin path to land on once the flow completes, instead of the
   *  homepage. Used by the guarded-route pattern: a restricted page sends the
   *  user here with `next` set to itself, so sign-in returns them to the page
   *  they were trying to reach. */
  next?: string;
}

/** Snapshot key carrying {@link RedirectInputs.next}. */
export const NEXT_KEY = "next";

/**
 * Narrows a `next` value to a same-origin path, or `undefined`.
 *
 * `next` reaches us through a URL, so it is attacker-craftable: without this
 * the callback page would forward the user anywhere after sign-in, i.e. it
 * would be an open redirect wearing this app's origin. Only a root-relative
 * path is accepted — never a full URL, protocol-relative `//host`, or anything
 * that could re-target the navigation to another origin.
 */
export const safeNextPath = (value: unknown): string | undefined => {
  if (typeof value !== "string" || !value.startsWith("/")) {
    return undefined;
  }
  // `//host` and `/\host` are protocol-relative — they navigate off-origin.
  if (value.startsWith("//") || value.startsWith("/\\")) {
    return undefined;
  }
  return value;
};

const text = (form: FormSnapshot, id: string): string => {
  const value = form[id];
  return typeof value === "string" ? value : "";
};

/** Derives the flow inputs from the form snapshot, by the homepage control ids. */
export const inputsFromSnapshot = (form: FormSnapshot): RedirectInputs => {
  const derivationOrigin = text(form, "derivationOrigin");
  const maxTtlRaw = text(form, "maxTimeToLive");
  const maxTtl = maxTtlRaw !== "" ? BigInt(maxTtlRaw) : BigInt(0);
  const nonce = text(form, "icrc3Nonce").trim();
  return {
    iiUrl: text(form, "iiUrl"),
    derivationOrigin: derivationOrigin !== "" ? derivationOrigin : undefined,
    maxTimeToLive: maxTtl > BigInt(0) ? maxTtl.toString() : undefined,
    requestAttributes: form.useIcrc3Attributes === true,
    attributeKeys: text(form, "requestAttributes")
      .split("\n")
      .map((s) => s.trim())
      .filter((s) => s.length > 0),
    nonce: nonce !== "" ? nonce : undefined,
    next: safeNextPath(form[NEXT_KEY]),
  };
};

const SNAPSHOT_PARAM = "form";

export const encodeSnapshot = (form: FormSnapshot): string => {
  const params = new URLSearchParams();
  params.set(SNAPSHOT_PARAM, JSON.stringify(form));
  return params.toString();
};

export const decodeSnapshot = (search: string): FormSnapshot => {
  const raw = new URLSearchParams(search).get(SNAPSHOT_PARAM);
  return raw !== null ? (JSON.parse(raw) as FormSnapshot) : {};
};

/** Results the callback page hands back to the homepage. The delegation itself
 *  isn't carried here — the homepage recovers it from `AuthClient`'s persisted
 *  session (as any redirect RP would). Only one-shot values ride the URL. */
export interface RedirectResults {
  /** Base64 ICRC-3 attribute data + signature, when attributes were requested. */
  attributes?: { data: string; signature: string };
  /** Error message when the flow failed. */
  error?: string;
  /** The form snapshot, echoed back so the homepage restores every option. The
   *  callback journals it via `memoize`, so it survives II's own redirect and is
   *  still available to hand back on the return leg. */
  form?: FormSnapshot;
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
  if (results.form !== undefined) {
    params.set("form", JSON.stringify(results.form));
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
  const form = params.get("form");
  return {
    error: params.get("error") ?? undefined,
    attributes:
      attributes !== null
        ? (JSON.parse(attributes) as RedirectResults["attributes"])
        : undefined,
    form: form !== null ? (JSON.parse(form) as FormSnapshot) : undefined,
  };
};
