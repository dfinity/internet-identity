import type { PageLoad } from "./$types";
import { z } from "zod";
import { fromBase64URL } from "$lib/utils/utils";

/** Default grant lifetime in seconds when the request omits `ttl`. */
const DEFAULT_TTL_SECONDS = 60 * 60;
/** Shortest TTL honoured: a request asking for less is clamped up to this, so a
 *  too-short value (e.g. a server still sending minutes-as-seconds) can't mint a
 *  uselessly-brief session. Matches the smallest duration the picker offers. In
 *  practice it shouldn't fire — the server is expected to send a sensible value
 *  (e.g. `ttl=3600` for 1 hour) — but it bounds a misbehaving caller. */
const MIN_TTL_SECONDS = 10 * 60;
/** Largest TTL the request can ask for, matching the longest duration the picker
 *  offers (30 days) and the backend's grant cap, so the backend never clamps
 *  further. */
const MAX_TTL_SECONDS = 30 * 24 * 60 * 60;

/**
 * The `/mcp` request, parsed from the URL fragment the MCP server redirects the
 * browser to. `valid` carries the validated request — the MCP server's
 * registration public key (`registration_key`, the per-connect key `X` the
 * server generated for this browser session), the callback identifying the
 * server, the single-use `state` the server echoes back so it can tie the
 * delivered delegation to the connect it started, and the requested
 * session-grant TTL (`ttl`, in seconds). The MCP server the user connects is
 * identified by the callback's *origin* (each user trusts whichever server they
 * connect); II mints a single-use registration delegation chain (rooted at a
 * canister-signed hop to a browser-held key, extended locally to `X`) and
 * delivers it to the callback only after it exact-matches the allow-list the
 * server declares at a fixed well-known path on that origin (see
 * `matchDeclaredCallback`), so a crafted link can't point II at an arbitrary
 * path on the trusted origin — the link only selects among the server-declared
 * callbacks. The only key material in the fragment is the server's own public
 * `registration_key` (never a secret), and no account is chosen here (it's
 * per-app, picked server-side at delegation time). `invalid` means the fragment
 * was missing or malformed.
 *
 * Whether the callback origin is one the connect flow accepts (https only — MCP
 * connections are to remote servers) is checked in the page component, which
 * shows a clean invalid screen.
 */
export type McpParams =
  | {
      kind: "valid";
      /** The MCP server's registration public key `X` (DER, base64url) for this
       *  connect. The registration chain's browser-signed final hop targets
       *  it, so the server can bind its long-lived session key by redeeming
       *  that chain (`mcp_register_v2`) — nothing secret rides this public
       *  key. */
      registrationKey: string;
      callback: string;
      /** Opaque value the server issued for this connect; delivered back
       *  alongside the registration delegation so the server can tie it to the
       *  connect it started (CSRF protection). */
      state: string;
      /** Requested session-grant lifetime in seconds (clamped to
       *  [10 min, 30 days]). */
      ttlSeconds: number;
    }
  | { kind: "invalid" };

/**
 * The whole fragment, validated and normalized in one schema. Absent params
 * arrive as `null` (from `URLSearchParams.get`), so any field typed plain
 * `z.string()` treats absence as invalid.
 */
const McpRequestSchema = z.object({
  // The server's registration public key `X` (DER, base64url). Kept verbatim
  // as the base64url string (decoded to bytes at connect time); this just
  // validates it is present and actually base64url-decodable, so a malformed
  // key invalidates the request up front rather than throwing mid-connect.
  registration_key: z.string().refine((raw) => {
    try {
      return fromBase64URL(raw).length > 0;
    } catch {
      return false;
    }
  }),
  // Structural callback check: must be an absolute http(s) URL. The stricter
  // "is this an origin the connect flow accepts" check (https only) happens in
  // the component, alongside the consent UI — keeping this `load` minimal
  // since it also runs at prerender.
  callback: z.string().refine((raw) => {
    try {
      const url = new URL(raw);
      return url.protocol === "https:" || url.protocol === "http:";
    } catch {
      return false;
    }
  }),
  state: z.string().min(1),
  // `ttl` is a lifetime in seconds. Any positive value is accepted and clamped
  // to the allowed range — at least 10 minutes, at most 30 days — so the exact
  // requested duration is honoured within those bounds. An omitted `ttl` uses
  // the default, and a malformed one (non-numeric or <= 0) invalidates the
  // request.
  ttl: z
    .string()
    .nullable()
    .transform((raw, ctx) => {
      if (raw === null) {
        return DEFAULT_TTL_SECONDS;
      }
      const parsed = Number(raw);
      if (!Number.isFinite(parsed) || parsed <= 0) {
        ctx.addIssue({
          code: "custom",
          message: "ttl must be a positive number of seconds",
        });
        return z.NEVER;
      }
      return Math.min(
        Math.max(Math.floor(parsed), MIN_TTL_SECONDS),
        MAX_TTL_SECONDS,
      );
    }),
});

export const load: PageLoad = ({ url }): { params: McpParams } => {
  // The MCP server redirects the browser here with the request in the URL
  // fragment (never sent to the server, so the callback and state stay out of
  // II's request logs; the address-bar copy is cleared in `+page.svelte` via
  // `replaceState`). Reading `url.hash` relies on this universal `load`
  // re-running client-side; with `adapter-static` it's empty during prerender.
  // The `registration_key` is the server's *public* per-connect key `X`; the
  // browser-signed hop of the registration chain targets it, so no secret key
  // material rides the (attacker-craftable) link.
  const params = new URLSearchParams(url.hash.slice(1));

  const request = McpRequestSchema.safeParse({
    registration_key: params.get("registration_key"),
    callback: params.get("callback"),
    state: params.get("state"),
    ttl: params.get("ttl"),
  });
  if (!request.success) {
    return { params: { kind: "invalid" } };
  }
  const { registration_key, callback, state, ttl } = request.data;
  return {
    params: {
      kind: "valid",
      registrationKey: registration_key,
      callback,
      state,
      ttlSeconds: ttl,
    },
  };
};
