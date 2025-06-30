import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
import { toaster } from "$lib/components/utils/toaster";
import { isCanisterError } from "$lib/utils/utils";
import type {
  CheckCaptchaError,
  IdRegFinishError,
  IdRegStartError,
  OpenIdCredentialAddError,
  OpenIdCredentialRemoveError,
} from "$lib/generated/internet_identity_types";
import { isOpenIdCancelError } from "$lib/utils/openID";

export const handleError = (error: unknown) => {
  // Handle browser errors
  if (isWebAuthnCancelError(error) || isOpenIdCancelError(error)) {
    toaster.info({
      title: "Operation canceled",
      description:
        "The interaction was canceled or timed out. Please try again.",
    });
    return;
  }

  // Handle canister errors
  if (
    isCanisterError<
      | IdRegStartError
      | IdRegFinishError
      | CheckCaptchaError
      | OpenIdCredentialAddError
      | OpenIdCredentialRemoveError
    >(error)
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
      case "OpenIdCredentialAlreadyRegistered":
        toaster.error({
          title: "This credential is already linked to another identity",
        });
        break;
      case "Unauthorized":
      case "InternalCanisterError":
        toaster.error({
          title: "An internal error occurred",
          description: error.value(error.type),
        });
        break;
      case "JwtVerificationFailed":
        toaster.error({
          title: "The JWT is invalid",
        });
        break;
      case "OpenIdCredentialNotFound":
        toaster.error({
          title: "This credential is not linked to this identity",
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
  console.error(error);
};
