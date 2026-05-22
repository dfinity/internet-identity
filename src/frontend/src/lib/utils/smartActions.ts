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

// Urgency drives the strip's left-to-right order. Each variant has
// its own constant: missing-recovery variants (`setup-*`) sit above
// `add-access-method` so the user is steered straight into setting
// up the recovery method that's still missing; already-configured
// variants (`update-email`, `reset-phrase`) sit below it. Within
// each tier the phrase variant ranks above the email variant.
//
// The promotion is binary — based purely on whether the method is
// configured at all — and intentionally does not reflect staleness
// (e.g. "your email is old, update it"). That is nudge logic and is
// handled (out of scope) by the dashboard's nudge component.
const URGENCY_SETUP_PHRASE = 110;
const URGENCY_SETUP_EMAIL = 100;
const URGENCY_ADD_ACCESS_METHOD = 50;
const URGENCY_RESET_PHRASE = 20;
const URGENCY_UPDATE_EMAIL = 10;

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
 * Each recovery rule picks exactly one of its two variants based on
 * whether the method is already configured (e.g. `setup-email` vs
 * `update-email`) and stamps it with that variant's per-action
 * urgency. The output is sorted by urgency descending; urgencies
 * are unique so the sort is fully deterministic and the same input
 * always produces the same output.
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
    actions.push(
      hasEmailRecovery(identityInfo)
        ? { id: "update-email", urgency: URGENCY_UPDATE_EMAIL }
        : { id: "setup-email", urgency: URGENCY_SETUP_EMAIL },
    );
  }

  actions.push(
    hasRecoveryPhrase(identityInfo)
      ? { id: "reset-phrase", urgency: URGENCY_RESET_PHRASE }
      : { id: "setup-phrase", urgency: URGENCY_SETUP_PHRASE },
  );

  return actions.sort((a, b) => b.urgency - a.urgency);
};
