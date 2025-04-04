import { Funnel } from "./Funnel";

/**
 * Webauthn Authorization flow events:
 *
 * start-webauthn-authentication (INIT)
 *   failed-webauthn-authentication
 *     cancelled-webauthn-authentication
 *   successful-webauthn-authentication
 */
export enum WebauthnAuthorizationEvents {
  Failed = "failed-webauthn-authentication",
  Cancelled = "cancelled-webauthn-authentication",
  Success = "successful-webauthn-authentication",
}

export const webauthnAuthorizationFunnel = new Funnel<
  typeof WebauthnAuthorizationEvents
>("webauthn-authentication");
