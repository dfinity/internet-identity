import { Funnel } from "./Funnel";

/**
 * Upgrade identity flow events:
 *
 * upgrade-identity-start (INIT)
 *   upgrade-authentication-successful
 *     create-passkey-screen
 *       upgrade-successful
 *       upgrade-failure
 *     already-migrated-screen
 *   upgrade-authentication-failure
 *     upgrade-authentication-cancelled
 */
export const UpgradeIdentityEvents = {
  AuthenticationSuccessful: "upgrade-authentication-successful",
  CreatePasskeyScreen: "create-passkey-screen",
  UpgradeSuccessful: "upgrade-successful",
  UpgradeFailure: "upgrade-failure",
  AlreadyMigratedScreen: "already-migrated-screen",
  AuthenticationFailure: "upgrade-authentication-failure",
  AuthenticationCancelled: "upgrade-authentication-cancelled",
} as const;

export const upgradeIdentityFunnel = new Funnel<typeof UpgradeIdentityEvents>(
  "upgrade-identity",
);
