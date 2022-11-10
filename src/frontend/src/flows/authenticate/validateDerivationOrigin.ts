import { Principal } from "@dfinity/principal";
import { wrapError } from "../../utils/utils";

const ORIGIN_VALIDATION_REGEX = /^https:\/\/([\w-]+)(?:\.raw)?\.ic0\.app$/;
const MAX_ALTERNATIVE_ORIGINS = 10;
type ValidationResult =
  | { result: "valid" }
  | { result: "invalid"; message: string };

/**
 * Function to validate the derivationOrigin. The derivationOrigin allows an application to request principals of a
 * different origin, given that origin allows this by listing the requesting application origin in the
 * .well-known/ii-alternative-origins resource.
 * See the spec for more details: https://github.com/dfinity/internet-identity/blob/main/docs/internet-identity-spec.adoc#alternative-frontend-origins
 *
 * This feature is currently experimental and for now limited in scope:
 * - only URLs matching the {@link ORIGIN_VALIDATION_REGEX} are allowed
 * @param authRequestOrigin Origin of the application requesting a delegation
 * @param derivationOrigin Origin to use for the principal derivation for this delegation
 */
export const validateDerivationOrigin = async (
  authRequestOrigin: string,
  derivationOrigin?: string
): Promise<ValidationResult> => {
  if (
    derivationOrigin === undefined ||
    derivationOrigin === authRequestOrigin
  ) {
    // this is the default behaviour -> no further validation necessary
    return { result: "valid" };
  }

  // check format of derivationOrigin
  const matches = ORIGIN_VALIDATION_REGEX.exec(derivationOrigin);
  if (matches === null) {
    return {
      result: "invalid",
      message: `derivationOrigin does not match regex ${ORIGIN_VALIDATION_REGEX.toString()}`,
    };
  }

  try {
    if (matches.length < 2) {
      return {
        result: "invalid",
        message: "invalid regex match result. No value for capture group 1.",
      };
    }
    // verifies that a valid principal id was matched
    // (canister ids must be valid principal ids)
    const canisterId = Principal.fromText(matches[1]);
    const alternativeOriginsUrl = `https://${canisterId.toText()}.ic0.app/.well-known/ii-alternative-origins`;
    const response = await fetch(
      // always fetch non-raw
      alternativeOriginsUrl,
      // fail on redirects
      {
        redirect: "error",
        headers: {
          Accept: "application/json",
        },
        // do not send cookies or other credentials
        credentials: "omit",
      }
    );

    if (response.status !== 200) {
      return {
        result: "invalid",
        message: `resource ${alternativeOriginsUrl} returned invalid status: ${response.status}`,
      };
    }

    const alternativeOriginsObj = (await response.json()) as {
      alternativeOrigins: string[];
    };

    // check for expected property
    if (!Array.isArray(alternativeOriginsObj?.alternativeOrigins)) {
      return {
        result: "invalid",
        message: `resource ${alternativeOriginsUrl} has invalid format: received ${alternativeOriginsObj}`,
      };
    }

    // check number of entries
    if (
      alternativeOriginsObj.alternativeOrigins.length > MAX_ALTERNATIVE_ORIGINS
    ) {
      return {
        result: "invalid",
        message: `Resource ${alternativeOriginsUrl} has too many entries: To prevent misuse at most ${MAX_ALTERNATIVE_ORIGINS} alternative origins are allowed.`,
      };
    }

    // check allowed alternative origins
    if (!alternativeOriginsObj.alternativeOrigins.includes(authRequestOrigin)) {
      return {
        result: "invalid",
        message: `"${authRequestOrigin}" is not listed in the list of allowed alternative origins. Allowed alternative origins: ${alternativeOriginsObj.alternativeOrigins}`,
      };
    }
  } catch (e) {
    return {
      result: "invalid",
      message: `An error occurred while validating the derivationOrigin "${derivationOrigin}": ${wrapError(
        e
      )}`,
    };
  }

  // all checks passed --> valid
  return { result: "valid" };
};
