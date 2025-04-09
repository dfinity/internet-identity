import { Funnel } from "./Funnel";

/**
 * Login flow events:
 *
 * Square brackets [] indicate optional events.
 *
 * login-start (INIT) - Triggered in Landing Page or List of identities
 *   identities-list | no-identities
 *     login-trigger-list-item - Triggered when user clicks on a number
 *       login-webauthn-start - Triggered when the webauthn is triggered
 *         login-success - Triggered after successful webauthn interaction
 *     go-use-existing - Triggered on visiting the Use Existing page
 *       login-trigger-use-existing - Triggered when user clicks "Continue" in the Use Existing
 *         login-webauthn-start - Triggered when the webauthn is triggered
 *           login-success - Triggered after successful webauthn interaction
 */
export enum LoginEvents {
  GoUseExisting = "go-use-existing",
  IdentitiesList = "identities-list",
  NoIdentities = "no-identities",
  TriggerListItem = "login-trigger-list-item",
  TriggerUseExisting = "login-trigger-use-existing",
  WebauthnStart = "login-webauthn-start",
  Success = "login-success",
}

export const loginFunnel = new Funnel<typeof LoginEvents>("login");
