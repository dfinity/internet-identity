import { toaster } from "$lib/components/utils/toaster";
import { t } from "$lib/stores/locale.store";
import { failureMessage } from "$lib/utils/notifications/notificationDiagnostics";
import { isCanisterError } from "$lib/utils/utils";

/** Tells the user the notification check after a sign-in failed, and why. A canister
 *  refusal carries no message, so its variant is the whole of the reason. */
export const showRepairFailure = (failure: unknown): void => {
  toaster.error({
    title: t`Notifications unavailable`,
    description: isCanisterError(failure)
      ? String(failure.type)
      : failureMessage(failure),
  });
};
