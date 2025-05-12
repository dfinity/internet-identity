import { isWebAuthnCancel } from "$lib/utils/webAuthnErrorUtils";
import { toaster } from "$lib/components/utils/toaster";
import { isCanisterError } from "$lib/utils/utils";
import type {
  CheckCaptchaError,
  IdRegFinishError,
  IdRegStartError,
} from "$lib/generated/internet_identity_types";
import { isPermissionError } from "$lib/utils/openID";

export const handleError = (error: unknown) => {
  // Handle browser errors
  if (isWebAuthnCancel(error)) {
    toaster.info({
      title: "Operation canceled",
      description:
        "The interaction was canceled or timed out. Please try again.",
    });
    return;
  }
  if (isPermissionError(error)) {
    toaster.error({
      title: "Permission denied",
      description:
        'You need to enable the "Third-party sign-in" browser permission for this site',
    });
    return;
  }

  // Handle canister errors
  if (
    isCanisterError<IdRegStartError | IdRegFinishError | CheckCaptchaError>(
      error,
    )
  ) {
    switch (error.type) {
      case "RateLimitExceeded":
      case "IdentityLimitReached":
        toaster.error({
          title: "It seems like registration is unavailable at this moment",
        });
        break;
      case "InvalidCaller":
      case "UnexpectedCall":
      case "NoRegistrationFlow":
        toaster.error({
          title: "Something went wrong during registration",
        });
        break;
      case "InvalidAuthnMethod":
      case "StorageError":
        toaster.error({
          title: "Something went wrong during registration",
          description: error.value(error.type),
        });
        break;
      case "AlreadyInProgress":
      case "WrongSolution":
        // Should be handled up the stack; reaching here means they weren’t.
        toaster.error({
          title: "Unhandled error",
          description: error.type,
        });
        break;
      default: {
        void (error.type satisfies never);
      }
    }
    return;
  }

  // Handle unexpected errors
  toaster.error({
    title: "Unexpected error",
    description: error instanceof Error ? error.message : undefined,
  });
};
