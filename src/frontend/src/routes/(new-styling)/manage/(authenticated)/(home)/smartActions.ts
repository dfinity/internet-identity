import type { IdentityInfo } from "$lib/generated/internet_identity_types";
import { getMetadataString } from "$lib/utils/openID";

export type SmartActionId =
  | "add-access-method"
  | "setup-email"
  | "update-email"
  | "setup-phrase"
  | "verify-phrase"
  | "reset-phrase";

export type SmartAction = {
  id: SmartActionId;
  urgency: number;
};

export type DeriveSmartActionsOptions = {
  emailRecoveryEnabled: boolean;
};

// Urgency drives the strip's left-to-right order. Each variant has
// its own constant. `verify-phrase` is on top — an unverified
// recovery phrase means the user holds words that may or may not
// actually unlock the identity, so verifying takes priority over
// every other action. Missing-recovery variants (`setup-*`) sit
// above `add-access-method` so the user is steered straight into
// completing the recovery method that's still missing;
// already-configured variants (`update-email`, `reset-phrase`) sit
// below it. Within each tier the phrase variant ranks above the
// email variant.
//
// Promotion is based purely on whether the method is configured at
// all (and, for the phrase, whether it has been verified). It
// intentionally does not reflect staleness (e.g. "your email is
// old, update it"); that's nudge logic, handled (out of scope) by
// the dashboard's nudge component.
const URGENCY_VERIFY_PHRASE = 200;
const URGENCY_SETUP_PHRASE = 110;
const URGENCY_SETUP_EMAIL = 100;
const URGENCY_ADD_ACCESS_METHOD = 50;
const URGENCY_RESET_PHRASE = 20;
const URGENCY_UPDATE_EMAIL = 10;

type RecoveryPhraseState = "none" | "unverified" | "verified";

const getRecoveryPhraseState = (
  identityInfo: IdentityInfo,
): RecoveryPhraseState => {
  const method = identityInfo.authn_methods.find(
    (m) =>
      "Recovery" in m.security_settings.purpose &&
      getMetadataString(m.metadata, "usage") === "recovery_phrase",
  );
  if (method === undefined) return "none";
  // A locked (Protected) phrase is treated as verified — locking it
  // required re-typing the phrase, which already proves the user
  // holds the right words. Mirrors the rule on the recovery page.
  const isLocked = "Protected" in method.security_settings.protection;
  const neverUsed = method.last_authentication[0] === undefined;
  return neverUsed && !isLocked ? "unverified" : "verified";
};

const hasEmailRecovery = (identityInfo: IdentityInfo): boolean =>
  identityInfo.email_recovery[0] !== undefined;

/**
 * Derives a contextual list of dashboard actions for the user's
 * current identity state.
 *
 * Each recovery rule picks exactly one variant based on the state
 * of its underlying method (e.g. `setup-email` vs `update-email`;
 * `setup-phrase` vs `verify-phrase` vs `reset-phrase`) and stamps
 * it with that variant's per-action urgency. The output is sorted
 * by urgency descending; urgencies are unique so the sort is fully
 * deterministic and the same input always produces the same output.
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

  switch (getRecoveryPhraseState(identityInfo)) {
    case "none":
      actions.push({ id: "setup-phrase", urgency: URGENCY_SETUP_PHRASE });
      break;
    case "unverified":
      actions.push({ id: "verify-phrase", urgency: URGENCY_VERIFY_PHRASE });
      break;
    case "verified":
      actions.push({ id: "reset-phrase", urgency: URGENCY_RESET_PHRASE });
      break;
  }

  return actions.sort((a, b) => b.urgency - a.urgency);
};
