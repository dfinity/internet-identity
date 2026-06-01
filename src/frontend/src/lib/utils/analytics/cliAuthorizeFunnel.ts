import { Funnel } from "./Funnel";

/**
 * /cli CLI-authorize flow events:
 *
 * start-cli-authorize (INIT)
 *   cli-authorize-request-invalid   (fragment missing or malformed)
 *   cli-authorize-request-received  (valid request, authorize screen shown)
 *     cli-authorize-confirmed       (user clicked Continue / Allow access)
 *       cli-authorize-cli-disabled  (app mode, CLI access not enabled here)
 *       cli-authorize-success       (loopback redirected back status=success)
 *       cli-authorize-error         (loopback redirected back status=error)
 *       cli-authorize-identity-mismatch (re-auth principal mismatch, retry)
 * end-cli-authorize
 *
 * The flow navigates away to the loopback server and back, so the success,
 * error, and identity-mismatch events fire on a fresh page load — Plausible
 * correlates them with the start by visitor session, not by JS lifetime.
 */
export const CliAuthorizeEvents = {
  RequestInvalid: "cli-authorize-request-invalid",
  RequestReceived: "cli-authorize-request-received",
  Confirmed: "cli-authorize-confirmed",
  CliDisabled: "cli-authorize-cli-disabled",
  Success: "cli-authorize-success",
  Error: "cli-authorize-error",
  IdentityMismatch: "cli-authorize-identity-mismatch",
} as const;

export const cliAuthorizeFunnel = new Funnel<typeof CliAuthorizeEvents>(
  "cli-authorize",
);
