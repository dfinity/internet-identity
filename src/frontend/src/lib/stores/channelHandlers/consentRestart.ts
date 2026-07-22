import {
  type Authorized,
  authorizedStore,
} from "$lib/stores/authorization.store";
import {
  type AttributeConsent,
  attributeConsentResultStore,
} from "$lib/stores/attributeConsent.store";

export type ConsentOutcome =
  | { type: "consent"; consent: AttributeConsent }
  | { type: "restart" };

/**
 * Block until either the user commits their choice on the consent screen, or
 * they switch identity. The consent screen keeps the identity switcher live,
 * and switching clears the authorization (dropping back to account selection
 * for the new identity) — the `authorizedStore` entry changes away from
 * `baseline`. The consent handler uses `"restart"` to re-resolve from
 * scratch for the newly selected identity instead of certifying the previous
 * identity's selections.
 */
export const waitForConsentOrRestart = (
  baseline: Authorized,
): Promise<ConsentOutcome> =>
  new Promise((resolve) => {
    let settled = false;
    const settle = (outcome: ConsentOutcome): void => {
      if (settled) return;
      settled = true;
      queueMicrotask(() => {
        unsubscribeConsent();
        unsubscribeAuthorized();
      });
      resolve(outcome);
    };
    const unsubscribeConsent = attributeConsentResultStore.subscribe(
      (consent) => {
        if (consent !== undefined) settle({ type: "consent", consent });
      },
    );
    const unsubscribeAuthorized = authorizedStore.subscribe((authorized) => {
      if (authorized !== baseline) {
        settle({ type: "restart" });
      }
    });
  });
