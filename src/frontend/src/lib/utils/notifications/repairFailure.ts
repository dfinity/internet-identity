import { toaster } from "$lib/components/utils/toaster";
import { t } from "$lib/stores/locale.store";
import { failureMessage } from "$lib/utils/notifications/notificationDiagnostics";

/** Tells the user the notification check after a sign-in failed, and why. */
export const showRepairFailure = (failure: unknown): void => {
  toaster.error({
    title: t`Notifications unavailable`,
    description: failureMessage(failure),
  });
};
