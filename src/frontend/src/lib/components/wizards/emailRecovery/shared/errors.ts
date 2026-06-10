import type {
  EmailRecoveryError,
  EmailRecoveryStatus,
} from "$lib/generated/internet_identity_types";

/**
 * The `Failed` reasons that can actually reach the failed view. The
 * poll loop intercepts `DomainNotAllowlisted` / `DomainNotSupported`
 * first and routes them to the dedicated unsupported-domain view, so
 * they never need a friendly string here. Excluding them lets
 * `friendlyFailedReason` stay exhaustive over everything that remains.
 */
export type FailedReason = Exclude<
  EmailRecoveryError,
  { DomainNotAllowlisted: string } | { DomainNotSupported: string }
>;

/**
 * Map a terminal `Failed` reason to a user-facing sentence, shared by
 * both the setup and recovery wizards.
 *
 * Exhaustive over `FailedReason`: the trailing `satisfies never` makes
 * the build fail if a new `EmailRecoveryError` variant is added (or one
 * is moved out of the unsupported-domain routing) until it's given
 * copy — instead of silently falling through to a raw variant name.
 *
 * `AddressAlreadyRegistered` only occurs in the setup flow and
 * `AddressNotRegistered` only in recovery; covering both here is
 * harmless (each is unreachable in the other flow) and keeps this a
 * single shared function rather than two near-identical copies.
 */
export const friendlyFailedReason = (reason: FailedReason): string => {
  if ("AddressNotRegistered" in reason) {
    return "We don't recognise this email. If you haven't registered it as a recovery method yet, sign in with another method first and add it.";
  }
  if ("AddressAlreadyRegistered" in reason) {
    return "This email is already used to recover a different identity.";
  }
  if ("AddressMismatch" in reason) {
    return "The email came from a different address than the one we have on file.";
  }
  if ("SubjectNotSigned" in reason) {
    return "Your email provider didn't sign the Subject header. Try a different provider.";
  }
  if ("DkimLeafMismatch" in reason) {
    return "Your email provider rotated its DKIM keys mid-flow. Please retry.";
  }
  if ("EmptyDkimLeafHops" in reason) {
    return "Internal error: the DKIM leaf request was malformed. Please retry.";
  }
  if ("NoDkimLeafExpected" in reason) {
    return "Internal error: the DKIM leaf was submitted at the wrong moment. Please retry.";
  }
  if ("EmailVerificationFailed" in reason) {
    return `Your email didn't verify (${reason.EmailVerificationFailed}). Make sure you sent it from the address you typed, no forwarding, no aliases.`;
  }
  if ("DohFetchFailed" in reason) {
    return "Something went wrong on our end. Please try again.";
  }
  if ("InternalCanisterError" in reason) {
    return `Something went wrong on our end: ${reason.InternalCanisterError}`;
  }
  if ("Unauthorized" in reason) {
    return "You're not authorised to change this identity's recovery email.";
  }
  if ("NonceUnknown" in reason) {
    return "This recovery link is no longer valid. Please try again.";
  }
  if ("NonceExpired" in reason) {
    return "This recovery link timed out. Please try again.";
  }
  return reason satisfies never;
};

/** Friendly copy for the status-level `Expired` (the pending challenge's
 *  30-minute TTL lapsed). Distinct from the `NonceExpired` error
 *  variant above, but the user sees the same thing. */
export const EXPIRED_MESSAGE =
  "This recovery link timed out. Please try again.";

/**
 * Map a terminal `EmailRecoveryStatus` (`Failed` or `Expired`) to the
 * variant-name string used as the `reason` property on the Plausible
 * `*-failed` / `*-unsupported-domain` events. Falls back to `unknown`
 * so a partial candid-shape change doesn't drop the event entirely.
 */
export const plausibleFailureReason = (status: EmailRecoveryStatus): string => {
  if ("Failed" in status) {
    return Object.keys(status.Failed)[0] ?? "unknown";
  }
  if ("Expired" in status) {
    return "Expired";
  }
  return "unknown";
};

/**
 * Granular machine sub-reason for a DoH transport failure, or
 * `undefined` for any non-DoH failure. The `DohFetchFailed` variant on
 * its own only says *that* DoH failed, not *why* — which matters most
 * for Gmail and other DoH-path providers. The canister now carries the
 * cause as a typed `DohFailureReason` discriminant (see `map_doh_error`
 * in the backend), so we read it directly and report it as the stable
 * `snake_case` `doh_reason` property on the `*-failed` funnel event,
 * keeping the funnel segmentable.
 *
 * Exhaustive over `DohFailureReason`: the trailing `satisfies never`
 * makes the build fail if the backend adds a DoH cause until it's given
 * a token here.
 */
export const dohSubReason = (
  status: EmailRecoveryStatus,
): string | undefined => {
  if (!("Failed" in status)) return undefined;
  const reason = status.Failed;
  if (!("DohFetchFailed" in reason)) return undefined;
  const cause = reason.DohFetchFailed;
  if ("AllProvidersFailed" in cause) return "all_providers_failed";
  if ("DedupWaitTimeout" in cause) return "dedup_wait_timeout";
  if ("RetryBackoffActive" in cause) return "retry_backoff_active";
  if ("QuorumFailed" in cause) return "quorum_failed";
  if ("ResponseMalformed" in cause) return "response_malformed";
  return cause satisfies never;
};
