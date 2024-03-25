import { Principal } from "@dfinity/principal";
import { isNullish } from "@dfinity/utils";

/**
 * Resolve the canister id of a canister based on the front-end origin.
 * @param origin The origin of the front-end to resolve the canister id of.
 */
export const resolveIssuerCanisterId = ({
  origin,
}: {
  origin: string;
}): Promise<{ ok: string } | "not_found"> => {
  const url = new URL(origin);

  if (url.hostname.endsWith("localhost")) {
    // The issuer is running on a local development environment, infer the canister ID from the hostname directly
    // (e.g. http://bd3sg-teaaa-aaaaa-qaaba-cai.localhost:4943 -> bd3sg-teaaa-aaaaa-qaaba-cai)
    const domainParts = url.hostname.split(".");

    // If there is no subdomain, we cannot infer the canister id
    if (domainParts.length > 1) {
      const canisterId = domainParts[0];
      try {
        Principal.fromText(canisterId); // make sure the inferred part is actually a canister id, throws if not
        return Promise.resolve({ ok: canisterId });
      } catch (e) {
        console.warn(
          `Unable to infer issuer canister id from origin ${origin}: ${e}`
        );
      }
    }
  }

  // Look up the canister id by performing a request to the origin
  return lookupCanister({ origin });
};

// Lookup the canister by performing a request to the origin and check
// if the server (probably BN) set a header to inform us of the canister ID
const lookupCanister = async ({
  origin,
}: {
  origin: string;
}): Promise<{ ok: string } | "not_found"> => {
  const response = await fetch(
    origin,
    // fail on redirects
    {
      redirect: "error",
      method: "HEAD",
      // do not send cookies or other credentials
      credentials: "omit",
    }
  );

  if (response.status !== 200) {
    console.error("Bad response when looking for canister ID", response.status);
    return "not_found";
  }

  const HEADER_NAME = "x-ic-canister-id";
  const canisterId = response.headers.get(HEADER_NAME);

  if (isNullish(canisterId)) {
    console.error(
      `Canister ID header '${HEADER_NAME}' was not set on origin ${origin}`
    );

    return "not_found";
  }

  return { ok: canisterId };
};
