import { Funnel } from "./Funnel";

/**
 * Authorize client flow events:
 *
 * authorize-client-start (INIT)
 *   authorize-client-request-received
 *     authorize-client-request-valid
 *       authorize-client-authenticate
 *         authorize-client-authenticate-success
 *         authorize-client-authenticate-error
 */
export const AuthorizeClientEvents = {
  RequestReceived: "authorize-client-request-received",
  RequestValid: "authorize-client-request-valid",
  Authenticate: "authorize-client-authenticate",
  AuthenticateError: "authorize-client-authenticate-error",
  AuthenticateSuccess: "authorize-client-authenticate-success",
} as const;

export const authorizeClientFunnel = new Funnel<typeof AuthorizeClientEvents>(
  "authorize-client",
);
