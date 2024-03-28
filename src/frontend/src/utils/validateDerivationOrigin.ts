import { wrapError } from "$src/utils/utils";
import { Principal } from "@dfinity/principal";
import { isNullish } from "@dfinity/utils";
import { resolveCanisterId } from "$src/utils/canisterIdResolution";
import { inferHost } from "$src/utils/iiConnection";

// Regex that's used to ensure an alternative origin is valid. We only allow canisters as alternative origins.
// Note: this allows origins that are served both from the legacy domain (ic0.app) and the official domain (icp0.io).
const ORIGIN_VALIDATION_REGEX =
  /^https:\/\/([\w-]+)(?:\.raw)?\.(?:ic0\.app|icp0\.io)$/;

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
  if (isNullish(derivationOrigin) || derivationOrigin === authRequestOrigin) {
    // this is the default behaviour -> no further validation necessary
    return { result: "valid" };
  }

  try {
    const canisterIdResult = await resolveCanisterId({origin: derivationOrigin})
    if (canisterIdResult === "not_found") {
      return {
        result: "invalid",
        message: `Could not resolve canister id for derivationOrigin "${derivationOrigin}".`,
      };
    }
    canisterIdResult satisfies {ok: string};

    // We always query the list of alternative origins from a canister id based URL in order to make sure that the request
    // is made through a BN that checks certification. Some flexibility is allowed by `inferHost` to allow for dev setups.
    const alternativeOriginsUrl = `https://${canisterIdResult.ok}.${inferHost()}/.well-known/ii-alternative-origins`;
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

const inferAlternativeOriginsUrl = (): string => {
  // The domain used for the HTTP
  const IC_HTTP_GATEWAY_DOMAIN = "icp0.io";

  const location = window?.location;
  if (isNullish(location)) {
    // If there is no location, then most likely this is a non-browser environment. All bets
    // are off, but we return something valid just in case.
    return "https://" + IC_HTTP_GATEWAY_DOMAIN;
  }

  if (
    location.hostname.endsWith("icp0.io") ||
    location.hostname.endsWith("ic0.app") ||
    location.hostname.endsWith("internetcomputer.org")
  ) {
    // If this is a canister running on one of the official IC domains, then return the
    // official API endpoint
    return "https://" + IC_HTTP_GATEWAY_DOMAIN;
  }

  if (
    location.host === "127.0.0.1" /* typical development */ ||
    location.host ===
    "0.0.0.0" /* typical development, though no secure context (only usable with builds with WebAuthn disabled) */ ||
    location.hostname.endsWith(
      "localhost"
    ) /* local canisters from icx-proxy like rdmx6-....-foo.localhost */
  ) {
    return "localhost"
  }

  // Otherwise assume it's a custom setup expecting the gateway to be on the same domain
  return location.hostname;
}
