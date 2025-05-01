import { Funnel } from "./Funnel";

/**
 * Authorize client flow events:
 *
 * authorize-client-start (INIT)
 *   authorize-client-orphan
 *   authorize-client-closed
 *   authorize-client-request-timeout
 *   authorize-client-request-invalid
 *   authorize-client-request-received
 *     authorize-client-authenticate
 *       authorize-client-authenticate-success
 *       authorize-client-authenticate-error
 */
export const AuthorizeClientEvents = {
  Orphan: "authorize-client-orphan",
  Closed: "authorize-client-closed",
  RequestTimeout: "authorize-client-request-timeout",
  RequestInvalid: "authorize-client-request-invalid",
  RequestReceived: "authorize-client-request-received",
  Authenticate: "authorize-client-authenticate",
  AuthenticateError: "authorize-client-authenticate-error",
  AuthenticateSuccess: "authorize-client-authenticate-success",
} as const;

export const authorizeClientFunnel = new Funnel<typeof AuthorizeClientEvents>(
  "authorize-client-v2",
);
