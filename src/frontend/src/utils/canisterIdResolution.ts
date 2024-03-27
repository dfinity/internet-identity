import { Principal } from "@dfinity/principal";
import { isNullish, nonNullish } from "@dfinity/utils";

/**
 * Resolve the canister id of a canister based on the front-end origin.
 * @param origin The origin of the front-end to resolve the canister id of.
 */
export const resolveCanisterId = ({
  origin,
}: {
  origin: string;
}): Promise<{ ok: string } | "not_found"> => {
  const url = new URL(origin);

  const maybeCanisterId = parseCanisterIdFromHostname(url.hostname);
  if (nonNullish(maybeCanisterId)) {
    return Promise.resolve({ ok: maybeCanisterId.toText() });
  }

  // Look up the canister id by performing a request to the origin
  return lookupCanister({ origin });
};

const parseCanisterIdFromHostname = (
  hostname: string
): Principal | undefined => {
  const wellKnownDomains = [
    "ic0.app",
    "icp0.io",
    "internetcomputer.org",
    "localhost",
  ];

  if (wellKnownDomains.some((domain) => hostname.endsWith(domain))) {
    // The canister is running on a well-known domain, infer the canister ID from the hostname directly
    // (e.g. bd3sg-teaaa-aaaaa-qaaba-cai.localhost -> bd3sg-teaaa-aaaaa-qaaba-cai)
    const domainParts = hostname.split(".");

    // If there is no subdomain, we cannot infer the canister id
    if (domainParts.length > 1) {
      const canisterId = domainParts[0];
      try {
        return Principal.fromText(canisterId); // make sure the inferred part is actually a canister id, throws if not
      } catch {
        console.info(
          `Unable to infer canister id from hostname '${hostname}', falling back to BN lookup.}`
        );
      }
    }
  }
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
