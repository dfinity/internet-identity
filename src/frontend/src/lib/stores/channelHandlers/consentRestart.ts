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
