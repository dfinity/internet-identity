import idp_actor from "../utils/idp_actor";
import {
  DelegationChain,
  WebAuthnIdentity,
  Delegation,
  SignedDelegation,
  Ed25519PublicKey,
  Ed25519KeyIdentity,
} from "@dfinity/identity";
import { getDerEncodedPublicKey } from "../utils/auth";
import { canisterId as signingCanisterId } from "dfx-generated/idp_service";
import {
  Principal,
  blobFromUint8Array,
  derBlobFromBlob,
  blobFromHex,
  blobToHex,
} from "@dfinity/agent";

// types
export declare type OAuth2AccessTokenResponse = {
  access_token: string;
  token_type: "bearer";
  expires_in: number;
  state?: string;
  scope?: string;
};
/**
 * This should be compatible with OAuth 2.0 Authorization Request.
 * But, since it uses underscore keys not camelCase and it's a 'greatest common denominator' of what's really going on, it could be a bit lossy, so it's not necessarily what we want to pass around internally, it's just a standard interop/serialization format of sorts.
 */
export interface OAuth2AuthorizationRequest {
  response_type: string;
  client_id?: string;
  redirect_uri: string;
  scope?: string;
  state?: string;
  /** not in oauth2: see 'ic-id-protocol.md' */
  login_hint: string;
}

const THREE_DAYS = 1000 * 60 * 60 * 24 * 3;

function getOauthParams(
  searchParams: URLSearchParams
): OAuth2AuthorizationRequest | null {
  try {
    const { response_type, login_hint, redirect_uri } = parseOauthParams(
      searchParams
    );
    const scope = searchParams.get("scope");
    const authorizationRequest = Object.assign(
      {
        response_type,
        login_hint,
        redirect_uri: decodeURIComponent(redirect_uri),
      },
      scope === null ? {} : { scope }
    );
    for (const param of ["client_id", "scope", "state"]) {
      const value = searchParams.get(param);
      if (!value) continue;
      authorizationRequest[param] = value;
    }
    return authorizationRequest;
  } catch (error) {
    return null;
  }
}

// returns true only when all required url params are found
// does _NOT_ throw because that would break the page when non-oauth
function hasOauthParams(searchParams: URLSearchParams) {
  const login_hint = searchParams.get("login_hint");
  const redirect_uri = searchParams.get("redirect_uri");
  const response_type = searchParams.get("response_type") || "token";
  return !!(login_hint && redirect_uri && response_type);
}

function parseOauthParams(searchParams: URLSearchParams) {
  // login_hint is der-encoded public key as hex string
  const login_hint = searchParams.get("login_hint") || "";
  // redirect_uri should be URL-encoded string
  const redirect_uri = searchParams.get("redirect_uri") || "";
  // response_type should always be token
  const response_type = searchParams.get("response_type") || "token";
  return { response_type, login_hint, redirect_uri };
}

function checkURIForHash(maybeURL: string) {
  const url = new URL(maybeURL);
  if (!!url.hash) {
    throw Error("no url in hash allowed");
  }
}

// this function will run every page load to check for oauth query parameters
export default function () {
  const searchParams = new URLSearchParams(location.search);
  const params = getOauthParams(searchParams)!;
  // TODO: if the redirect_uri parameter has a hash, immediately reject
  if (hasOauthParams(searchParams)) {
    try {
      checkURIForHash(params.redirect_uri);
      //  check do we have identity?
      const identity = localStorage.getItem("identity");
      if (identity !== null) {
        // does the user consent?
        if (checkConsent(params?.scope)) {
          generate_access_token(
            WebAuthnIdentity.fromJSON(identity),
            params.login_hint
          ).then((access_token: string) => {
            redirectToApp(params.redirect_uri, {
              access_token: access_token,
              token_type: "bearer",
              expires_in: THREE_DAYS,
              scope: params.scope,
              state: params.state,
            });
          });
        } else {
          // TODO: redirect with failure message
          const url = new URL(params.redirect_uri);
          const search = new URLSearchParams({ error: "not authorized" });
          url.search += "&" + search;
          globalThis.location.assign(url.toString());
        }
      } else {
        // TODO: create or lookup identity
        createOrLookupIdentity(params);
      }
    } catch (error) {
      console.error(error);
      globalThis.alert(error.message + ", sending you back to application");
      globalThis.location.assign(params.redirect_uri);
    }
  } else {
    // do nothing, this will be handled by other logic
  }
}

// TODO: actually write this flow
function createOrLookupIdentity(params: OAuth2AuthorizationRequest) {}

// builds query parameters in accordance with oauth response types
// and navigates the user back to the app.
function redirectToApp(redirectURI: string, params: OAuth2AccessTokenResponse) {
  const responseURL = new URL(redirectURI);
  const searchParams = new URLSearchParams();

  // ignore keys which have no value associated
  Object.entries(params).forEach(([key, value]) => {
    if (value && typeof value === "string") {
      searchParams.append(key, value);
    }
  });
  responseURL.hash = `#${searchParams.toString()}`;

  // redirect to app
  globalThis.location.assign(responseURL.toString());
}

function checkConsent(scope?: string) {
  return prompt(
    `The following canisters are requesting access to your identity:\n\n${
      scope?.replaceAll(' ', '\n') || "UNKNOWN CANISTER"
    }\n\nAllow? [y/n]`
  )?.match(/y/i);
}

async function generate_access_token(
  identity: WebAuthnIdentity,
  login_hint: string
) {
  const res = await idp_actor.getDelegation();
  if (!isDelegationResponse(res)) {
    throw Error("got bad variant?");
  }

  const { delegation } = res;

  const rootToDeviceDelegation = {
    delegation: new Delegation(
      blobFromUint8Array(Uint8Array.from(delegation.delegation.pubkey)),
      BigInt(delegation.delegation.expiration)
    ),
    signature: blobFromUint8Array(Uint8Array.from(delegation.signature)),
  };

  const sessionKey = Ed25519PublicKey.fromDer(blobFromHex(login_hint));

  const userId = BigInt(localStorage.getItem("userId"));
  const publicKey = getDerEncodedPublicKey(
    userId,
    Principal.fromText(signingCanisterId)
  );

  const rootToDevice = DelegationChain.fromDelegations(
    [rootToDeviceDelegation],
    derBlobFromBlob(blobFromUint8Array(Uint8Array.from(publicKey)))
  );
  const rootToSession = await DelegationChain.create(
    identity,
    sessionKey,
    new Date(Date.now() + THREE_DAYS),
    { previous: rootToDevice }
  );

  console.log("Delegation chain");
  console.log(rootToSession);
  const chainJson = JSON.stringify(rootToSession.toJSON());
  console.log("Delegation chain JSON");
  console.log(chainJson);

  return new Buffer(chainJson).toString('hex');
}

function isDelegationResponse(x: any): x is { delegation: SignedDelegation } {
  return x && x.hasOwnProperty("delegation");
}

export const redirectBackWithAuthorization = () => {
  debugger;
};

