import { Principal } from "@dfinity/principal";

export type ValidationResult =
  | { result: "valid" }
  | { result: "invalid"; message: string };

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
  const matches = /^https:\/\/([\w-]*)(\.raw)?\.ic0\.app$/.exec(
    derivationOrigin
  );
  if (matches === null) {
    return {
      result: "invalid",
      message:
        'derivationOrigin does not match regex "^https:\\/\\/([\\w-]*)(\\.raw)?\\.ic0\\.app$"',
    };
  }

  try {
    const canisterId = Principal.fromText(matches[1]); // verifies that a valid canister id was matched
    const alternativeOriginsUrl = `https://${canisterId.toText()}.ic0.app/.well-known/ii-alternative-origins`;
    const response = await fetch(
      // always fetch non-raw
      alternativeOriginsUrl,
      // fail on redirects
      { redirect: "error" }
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
      message: `An error occurred while validation the derivationOrigin "${derivationOrigin}": ${e.message}`,
    };
  }

  // all checks passed --> valid
  return { result: "valid" };
};
