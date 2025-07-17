import { Funnel } from "./Funnel";

/**
 * Registration flow events:
 *
 * Square brackets [] indicate optional events.
 *
 * registration-start (INIT)
 *   identities-list | no-identities (come from loginFunnel)
 *     registration-trigger
 *       registration-webauthn-start
 *         [captcha-check]
 *           registration-created
 *             [copy-new-identity-number]
 *               registration-success
 */
export const RegistrationEvents = {
  Trigger: "registration-trigger",
  CaptchaCheck: "captcha-check",
  WebauthnStart: "registration-webauthn-start",
  Created: "registration-created",
  CopyNewIdentityNumber: "copy-new-identity-number",
  Success: "registration-success",
} as const;

export const registrationFunnel = new Funnel<typeof RegistrationEvents>(
  "registration",
);
