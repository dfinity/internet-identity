import { Funnel } from "./Funnel";

/**
 * Webauthn Authorization flow events:
 *
 * start-webauthn-authentication (INIT)
 *   failed-webauthn-authentication
 *     cancelled-webauthn-authentication
 *   successful-webauthn-authentication
 */
export enum WebauthnAuthenticationEvents {
  Failed = "failed-webauthn-authentication",
  Cancelled = "cancelled-webauthn-authentication",
  Success = "successful-webauthn-authentication",
}

export const webauthnAuthenticationFunnel = new Funnel<
  typeof WebauthnAuthenticationEvents
>("webauthn-authentication");
