import type { IdentityInfo } from "$lib/generated/internet_identity_types";
import { getMetadataString } from "$lib/utils/openID";

export type SmartActionId =
  | "add-access-method"
  | "setup-email"
  | "update-email"
  | "setup-phrase"
  | "reset-phrase";

export type SmartAction = {
  id: SmartActionId;
  urgency: number;
};

export type DeriveSmartActionsOptions = {
  emailRecoveryEnabled: boolean;
};

// Urgency drives the strip's left-to-right order; it is a per-action
// constant, independent of whether the user already has that method
// configured. State changes only swap the action's label (e.g.
// `setup-email` vs `update-email`) without re-prioritising it.
// Promoting "missing" actions is nudge logic and is intentionally
// not handled here.
const URGENCY_ADD_ACCESS_METHOD = 30;
const URGENCY_EMAIL = 20;
const URGENCY_PHRASE = 10;

const hasRecoveryPhrase = (identityInfo: IdentityInfo): boolean =>
  identityInfo.authn_methods.some(
    (m) =>
      "Recovery" in m.security_settings.purpose &&
      getMetadataString(m.metadata, "usage") === "recovery_phrase",
  );

const hasEmailRecovery = (identityInfo: IdentityInfo): boolean =>
  identityInfo.email_recovery[0] !== undefined;

/**
 * Derives a contextual list of dashboard actions for the user's
 * current identity state.
 *
 * Rules emit one action each; mutually exclusive variants (e.g.
 * `setup-email` vs `update-email`) collapse based on whether the
 * underlying recovery method is already configured. The output is
 * sorted by urgency descending. `Array.prototype.sort` is stable in
 * modern engines, so equal-urgency actions keep their insertion
 * order — meaning the same input always produces the same output.
 */
export const deriveSmartActions = (
  identityInfo: IdentityInfo,
  { emailRecoveryEnabled }: DeriveSmartActionsOptions,
): SmartAction[] => {
  const actions: SmartAction[] = [];

  actions.push({
    id: "add-access-method",
    urgency: URGENCY_ADD_ACCESS_METHOD,
  });

  if (emailRecoveryEnabled) {
    actions.push({
      id: hasEmailRecovery(identityInfo) ? "update-email" : "setup-email",
      urgency: URGENCY_EMAIL,
    });
  }

  actions.push({
    id: hasRecoveryPhrase(identityInfo) ? "reset-phrase" : "setup-phrase",
    urgency: URGENCY_PHRASE,
  });

  return actions.sort((a, b) => b.urgency - a.urgency);
};
