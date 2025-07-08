import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
import { toaster } from "$lib/components/utils/toaster";
import { isCanisterError } from "$lib/utils/utils";
import type {
  CheckCaptchaError,
  CreateAccountError,
  IdRegFinishError,
  IdRegStartError,
  OpenIdCredentialAddError,
  OpenIdCredentialRemoveError,
} from "$lib/generated/internet_identity_types";
import { isOpenIdCancelError } from "$lib/utils/openID";
import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";

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
      | CreateAccountError
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
      case "AccountLimitReached":
        toaster.warning({
          title: "Limit reached",
          description: "No more additional accounts can be created",
        });
        break;
      case "OpenIdCredentialAlreadyRegistered":
        toaster.error({
          title: "This account is already linked to another identity",
        });
        break;
      case "JwtVerificationFailed":
        toaster.error({
          title: "Authorization invalid",
          description: `It may have expired — please try again.`,
        });
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.JwtVerificationFailed,
          {
            error: error.value(error.type),
          },
        );
        break;
      case "OpenIdCredentialNotFound":
        toaster.error({
          title: "This account has already been unlinked",
        });
        break;
      case "AlreadyInProgress":
      case "WrongSolution":
        // Should be handled up the stack; reaching here means they weren't.
        toaster.error({
          title: "Unhandled error",
          description: error.type,
        });
        console.error(error);
        break;
      case "NameTooLong":
      case "Unauthorized":
        // Shouldn't have happened; reaching here means they weren't avoided.
        toaster.error({
          title: "Unexpected error",
          description: error.type,
        });
        console.error(error);
        break;
      case "InternalCanisterError":
        // Should never happen; reaching here means there's a technical issue.
        toaster.error({
          title: "An internal error occurred",
          description: error.value(error.type),
        });
        console.error(error);
        break;
      default: {
        // Should be avoided; reaching here means an error is missing above.
        void (error.type satisfies never);
        toaster.error({
          title: "Unknown error",
          description: error.type,
        });
        console.error(error);
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
