import { Funnel } from "./Funnel";

/**
 * /cli CLI-authorize flow events (the funnel prefixes each with `cli-authorize--`):
 *
 * start-cli-authorize (INIT)
 *   cli-authorize--request-invalid   (fragment missing or malformed)
 *   cli-authorize--request-received  (valid request, authorize screen shown)
 *     cli-authorize--confirmed       (user clicked Continue / Allow access)
 *       cli-authorize--cli-disabled  (app mode, CLI access not enabled here)
 *       cli-authorize--success       (loopback redirected back status=success)
 *       cli-authorize--error         (loopback redirected back status=error)
 *       cli-authorize--identity-mismatch (re-auth principal mismatch, retry)
 * end-cli-authorize
 *
 * The flow navigates away to the loopback server and back, so the success,
 * error, and identity-mismatch events fire on a fresh page load — Plausible
 * correlates them with the start by visitor session, not by JS lifetime.
 */
export const CliAuthorizeEvents = {
  RequestInvalid: "request-invalid",
  RequestReceived: "request-received",
  Confirmed: "confirmed",
  CliDisabled: "cli-disabled",
  Success: "success",
  Error: "error",
  IdentityMismatch: "identity-mismatch",
} as const;

export const cliAuthorizeFunnel = new Funnel<typeof CliAuthorizeEvents>(
  "cli-authorize",
  true,
);
