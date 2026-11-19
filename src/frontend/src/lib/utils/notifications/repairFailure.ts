import type {
  DisableNotificationAppError,
  GetNotificationPathStatusError,
  ProbeNotificationPathError,
  RemoveUnansweredPwaWebPushSubscriptionsError,
} from "$lib/generated/internet_identity_types";
import { toaster } from "$lib/components/utils/toaster";
import { t } from "$lib/stores/locale.store";
import { failureMessage } from "$lib/utils/notifications/notificationDiagnostics";
import { isCanisterError, type CanisterError } from "$lib/utils/utils";

/** The refusals the notification check's canister calls answer with. */
type RepairCallError =
  | GetNotificationPathStatusError
  | ProbeNotificationPathError
  | RemoveUnansweredPwaWebPushSubscriptionsError
  | DisableNotificationAppError;

/** A refusal's variant, with the reason the canister gave where it gave one. */
const refusalReason = (error: CanisterError<RepairCallError>): string =>
  error.type === "InternalCanisterError"
    ? `${error.type}: ${error.value("InternalCanisterError")}`
    : String(error.type);

/** Tells the user the notification check after a sign-in failed, and why. */
export const showRepairFailure = (failure: unknown): void => {
  toaster.error({
    title: t`Notifications unavailable`,
    description: isCanisterError<RepairCallError>(failure)
      ? refusalReason(failure)
      : failureMessage(failure),
  });
};
