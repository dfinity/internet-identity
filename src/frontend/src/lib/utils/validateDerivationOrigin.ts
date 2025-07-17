import { resolveCanisterId as resolveCanisterIdFn } from "$lib/utils/canisterIdResolution";
import { wrapError } from "$lib/utils/utils";
import { Principal } from "@dfinity/principal";
import { isNullish } from "@dfinity/utils";

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
 * @param authRequestOrigin Origin of the application requesting a delegation
 * @param derivationOrigin Origin to use for the principal derivation for this delegation
 */
export const validateDerivationOrigin = async ({
  requestOrigin,
  derivationOrigin,
  resolveCanisterId = resolveCanisterIdFn,
}: {
  requestOrigin: string;
  derivationOrigin?: string;
  resolveCanisterId?: typeof resolveCanisterIdFn;
}): Promise<ValidationResult> => {
  if (isNullish(derivationOrigin) || derivationOrigin === requestOrigin) {
    // this is the default behaviour -> no further validation necessary
    return { result: "valid" };
  }

  try {
    const canisterIdResult = await resolveCanisterId({
      origin: derivationOrigin,
    });
    if (canisterIdResult === "not_found") {
      return {
        result: "invalid",
        message: `Could not resolve canister id for derivationOrigin "${derivationOrigin}".`,
      };
    }
    canisterIdResult satisfies { ok: Principal };

    // We always query the list of alternative origins from a canister id based URL in order to make sure that the request
    // is made through a BN that checks certification.
    // Some flexibility is allowed by `inferAlternativeOriginsUrl` to allow for dev setups.
    const alternativeOriginsUrl = inferAlternativeOriginsUrl({
      canisterId: canisterIdResult.ok,
    });
    const response = await fetch(
      alternativeOriginsUrl,
      // fail on redirects
      {
        redirect: "error",
        headers: {
          Accept: "application/json",
        },
        // do not send cookies or other credentials
        credentials: "omit",
      },
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
    if (!alternativeOriginsObj.alternativeOrigins.includes(requestOrigin)) {
      return {
        result: "invalid",
        message: `"${requestOrigin}" is not listed in the list of allowed alternative origins. Allowed alternative origins: ${alternativeOriginsObj.alternativeOrigins}`,
      };
    }
  } catch (e) {
    return {
      result: "invalid",
      message: `An error occurred while validating the derivationOrigin "${derivationOrigin}": ${wrapError(
        e,
      )}`,
    };
  }

  // all checks passed --> valid
  return { result: "valid" };
};

/**
 * Infer the URL to fetch the alternative origins file from based on the canister id
 * and the current location.
 * Deployments on mainnet, (including production II hosted on ic0.app or internetcomputer.org) will always only use the
 * official icp0.io HTTP gateway.
 * Dev deployments hosted on localhost or custom domains will use the same domain as the current location.
 *
 * @param canisterId The canister id to fetch the alternative origins file from.
 */
const inferAlternativeOriginsUrl = ({
  canisterId,
}: {
  canisterId: Principal;
}): string => {
  // The official HTTP gateway
  // We never fetch from a custom domain or a raw URL in order to ensure that the request is made through a BN that checks certification.
  const IC_HTTP_GATEWAY_DOMAIN = "icp0.io";
  const ALTERNATIVE_ORIGINS_PATH = "/.well-known/ii-alternative-origins";

  const location = window?.location;
  if (isNullish(location)) {
    // If there is no location, then most likely this is a non-browser environment. All bets
    // are off, but we return something valid just in case.
    return `https://${canisterId.toText()}.${IC_HTTP_GATEWAY_DOMAIN}${ALTERNATIVE_ORIGINS_PATH}`;
  }

  // Local deployment -> add query parameter
  // For this asset the query parameter should work regardless of whether we use a canister id based subdomain or not
  if (location.hostname.endsWith("localhost")) {
    // on localhost, use `localhost` as the domain to avoid routing issues in case of canister id subdomain based routing

    // preserve the port if it's not the default
    const portSegment = location.port !== "" ? `:${location.port}` : "";

    return `${location.protocol}//localhost${portSegment}${ALTERNATIVE_ORIGINS_PATH}?canisterId=${canisterId}`;
  }
  // Preserve host when using IP addresses
  if (location.hostname === "127.0.0.1" || location.hostname === "0.0.0.0") {
    return `${location.protocol}//${location.host}${ALTERNATIVE_ORIGINS_PATH}?canisterId=${canisterId}`;
  }

  // Otherwise, assume it's a mainnet environment.
  return `https://${canisterId.toText()}.${IC_HTTP_GATEWAY_DOMAIN}${ALTERNATIVE_ORIGINS_PATH}`;
};
