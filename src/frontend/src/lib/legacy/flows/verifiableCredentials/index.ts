import type { SignedIdAlias } from "$lib/generated/internet_identity_types";
import { useIdentity } from "$lib/templates/authenticateBox";
import { infoToastTemplate } from "$lib/templates/infoToast";
import { withLoader } from "$lib/templates/loader";
import { showMessage } from "$lib/templates/message";
import { showSpinner } from "$lib/templates/spinner";
import { toast } from "$lib/templates/toast";
import { fetchDelegation } from "$lib/legacy/flows/authorize/fetchDelegation";
import { I18n } from "$lib/legacy/i18n";
import { getAnchorByPrincipal } from "$lib/legacy/storage";
import { AuthenticatedConnection, Connection } from "$lib/utils/iiConnection";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import {
  Delegation,
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@dfinity/identity";
import {
  CredentialSpec,
  IssuedCredentialData,
} from "@dfinity/internet-identity-vc-api";
import { Principal } from "@dfinity/principal";
import infoToastCopy from "$lib/templates/infoToast/copy.json";
import { abortedCredentials } from "./abortedCredentials";
import { allowCredentials } from "./allowCredentials";
import { VcVerifiablePresentation, vcProtocol } from "./postMessageInterface";
import { VcIssuer } from "./vcIssuer";

// The "verifiable credentials" approval flow
export const vcFlow = async ({
  connection,
}: {
  connection: Connection;
}): Promise<never> => {
  const result = await vcProtocol({
    /* Show some spinners while we wait for more data or for an action to complete */
    onProgress: (x) => {
      if (x === "waiting") {
        return showSpinner({
          message: "Waiting for verification data",
        });
      }

      if (x === "verifying") {
        return showSpinner({
          message: "Checking verification data",
        });
      }
      x satisfies never;
    },

    /* How the credentials are actually verified */
    verifyCredentials: (args) => verifyCredentials({ connection, ...args }),
  });

  if (result === "orphan") {
    await showMessage({
      message: "No credentials data provided! Wrong URL?",
    });
  }

  return await new Promise((_) => {
    /* halt forever */
  });
};

type VerifyCredentials = Parameters<typeof vcProtocol>[0]["verifyCredentials"];
type VerifyCredentialsArgs = Parameters<VerifyCredentials>[0];

const verifyCredentials = async ({
  connection,
  request: {
    credentialSubject: givenP_RP,
    issuer: { origin: issuerOrigin, canisterId: issuerCanisterId },
    credentialSpec,
    derivationOrigin: rpDerivationOrigin,
  },
  rpOrigin: rpOrigin_,
}: { connection: Connection } & VerifyCredentialsArgs) => {
  // Verify that principals may be issued to RP using the specified
  // derivation origin
  const validRpDerivationOrigin = await withLoader(() =>
    validateDerivationOrigin({
      requestOrigin: rpOrigin_,
      derivationOrigin: rpDerivationOrigin,
    }),
  );
  if (validRpDerivationOrigin.result === "invalid") {
    return abortedCredentials({ reason: "bad_derivation_origin_rp" });
  }
  validRpDerivationOrigin.result satisfies "valid";
  const rpOrigin = rpDerivationOrigin ?? rpOrigin_;

  const vcIssuer = new VcIssuer(issuerCanisterId);

  const issuerDerivationOriginResult = await getValidatedIssuerDerivationOrigin(
    {
      vcIssuer,
      issuerOrigin,
    },
  );

  if (issuerDerivationOriginResult.kind === "error") {
    return abortedCredentials({ reason: issuerDerivationOriginResult.err });
  }

  const issuerDerivationOrigin = issuerDerivationOriginResult.origin;

  // XXX: We don't check that the language matches the user's language. We need
  // to figure what to do UX-wise first.
  const consentInfo = await vcIssuer.getConsentMessage({ credentialSpec });

  if (consentInfo === "error") {
    return abortedCredentials({ reason: "auth_failed_issuer" });
  }

  const userNumber_ = await getAnchorByPrincipal({ principal: givenP_RP });

  // Ask user to confirm the verification of credentials
  const allowed = await allowCredentials({
    relyingOrigin:
      rpOrigin_ /* NOTE: the design does not show the derivation origin (yet) */,
    providerOrigin: issuerOrigin,
    consentMessage: consentInfo.consent_message,
    userNumber: userNumber_,
  });
  if (allowed.tag === "canceled") {
    return "aborted";
  }
  allowed.tag satisfies "allowed";

  const userNumber = allowed.userNumber;

  // For the rest of the flow we need to be authenticated, so authenticate
  let authResult = await useIdentity({
    userNumber,
    connection,
    allowPinLogin: true,
  });

  if ("tag" in authResult) {
    authResult satisfies { tag: "canceled" };
    return "aborted";
  }

  authResult satisfies { kind: unknown };

  // There are three supported origins. I wanted to give the user a chance to cancel once the correct one and maybe still make it work.
  // Therefore, the max retries should allow to iterate through all the 3 origins twice.
  const MAX_RETRIES = 5;
  let currentRetry = 0;
  // This is ugly, but I couldn't find a better way to handle the retry without disrupting the whole flow.
  while (
    authResult.kind === "possiblyWrongWebAuthnFlow" &&
    currentRetry < MAX_RETRIES
  ) {
    currentRetry++;
    const i18n = new I18n();
    const copy = i18n.i18n(infoToastCopy);
    toast.info(
      infoToastTemplate({
        title: copy.title_trying_again,
        messages: [copy.message_possibly_wrong_web_authn_flow_1],
      }),
    );
    authResult = await useIdentity({
      userNumber,
      connection,
      allowPinLogin: true,
    });

    if ("tag" in authResult) {
      authResult satisfies { tag: "canceled" };
      return "aborted";
    }

    authResult satisfies { kind: unknown };
  }

  if (authResult.kind !== "loginSuccess") {
    return abortedCredentials({ reason: "auth_failed_ii" });
  }
  const authenticatedConnection = authResult.connection;

  // Compute the user's principal on the RP and ensure it matches what the RP sent us
  const computedP_RP = await withLoader(() =>
    authenticatedConnection.getPrincipal({
      origin: rpOrigin,
    }),
  );
  if (computedP_RP.compareTo(givenP_RP) !== "eq") {
    console.error("Principal did not match that expected by RP");
    return abortedCredentials({ reason: "bad_principal_rp" });
  }

  // Ask II to generate the aliases

  const pAliasPending = getAliasCredentials({
    rpOrigin,
    issuerOrigin: issuerDerivationOrigin /* Use the actual derivation origin */,
    authenticatedConnection,
  });

  // Grab the credentials from the issuer
  const credentials = await withLoader(async () => {
    const pAliasRes = await pAliasPending;

    if ("err" in pAliasRes) {
      return { err: pAliasRes.err };
    }

    const pAlias = pAliasRes.ok;

    const issuedCredential = await issueCredential({
      vcIssuer,
      issuerOrigin: issuerDerivationOrigin,
      issuerAliasCredential: pAlias.issuerAliasCredential,
      credentialSpec,
      authenticatedConnection,
    });

    if ("err" in issuedCredential) {
      return { err: issuedCredential.err };
    }
    return [issuedCredential.ok, pAlias] as const;
  });

  if ("err" in credentials) {
    return abortedCredentials({ reason: credentials.err });
  }

  const [issuedCredential, pAlias] = credentials;

  // Create the presentation and return it to the RP
  return createPresentation({
    issuerCanisterId,
    rpAliasCredential: pAlias.rpAliasCredential,
    issuedCredential,
  });
};

// Prepare & get aliases
const getAliasCredentials = async ({
  authenticatedConnection,
  issuerOrigin,
  rpOrigin,
}: {
  issuerOrigin: string;
  rpOrigin: string;
  authenticatedConnection: AuthenticatedConnection;
}): Promise<
  | {
      ok: {
        rpAliasCredential: SignedIdAlias;
        issuerAliasCredential: SignedIdAlias;
      };
    }
  | { err: "internal_error" | "auth_failed_ii" }
> => {
  const preparedIdAlias = await authenticatedConnection.prepareIdAlias({
    issuerOrigin,
    rpOrigin,
  });

  if ("error" in preparedIdAlias) {
    if (preparedIdAlias.error === "internal_error") {
      console.error("Could not prepare ID alias");
      return { err: "internal_error" };
    }

    preparedIdAlias.error satisfies "authentication_failed";
    console.error("Could not prepare ID alias: authentication failed");
    return { err: "auth_failed_ii" };
  }

  const result = await authenticatedConnection.getIdAlias({
    preparedIdAlias,
    issuerOrigin,
    rpOrigin,
  });

  if ("error" in result) {
    if (result.error === "internal_error") {
      console.error("Could not get ID alias");
      return { err: "internal_error" };
    }

    result.error satisfies "authentication_failed";
    console.error("Could not get ID alias: authentication failed");
    return { err: "auth_failed_ii" };
  }

  const {
    rp_id_alias_credential: rpAliasCredential,
    issuer_id_alias_credential: issuerAliasCredential,
  } = result;

  return { ok: { rpAliasCredential, issuerAliasCredential } };
};

// Looks up and verify the derivation origin to be used by the issuer
const getValidatedIssuerDerivationOrigin = async ({
  vcIssuer,
  issuerOrigin,
}: {
  vcIssuer: VcIssuer;
  issuerOrigin: string;
}): Promise<
  | {
      kind: "error";
      err:
        | "derivation_origin_issuer_error"
        | "invalid_derivation_origin_issuer";
    }
  | { kind: "ok"; origin: string }
> => {
  const derivationOriginResult = await vcIssuer.getDerivationOrigin({
    origin: issuerOrigin,
  });

  if (derivationOriginResult.kind === "error") {
    return { kind: "error", err: "derivation_origin_issuer_error" };
  }
  derivationOriginResult.kind satisfies "origin";

  const validationResult = await validateDerivationOrigin({
    requestOrigin: issuerOrigin,
    derivationOrigin: derivationOriginResult.origin,
  });
  if (validationResult.result === "invalid") {
    console.error(
      `Invalid derivation origin ${derivationOriginResult.origin} for issuer ${issuerOrigin}: ${validationResult.message}`,
    );
    return { kind: "error", err: "invalid_derivation_origin_issuer" };
  }
  validationResult.result satisfies "valid";

  return { kind: "ok", origin: derivationOriginResult.origin };
};

// Contact the issuer to issue the credentials
const issueCredential = async ({
  vcIssuer,
  issuerOrigin,
  issuerAliasCredential,
  credentialSpec,
  authenticatedConnection,
}: {
  vcIssuer: VcIssuer;
  issuerOrigin: string;
  issuerAliasCredential: SignedIdAlias;
  credentialSpec: CredentialSpec;
  authenticatedConnection: AuthenticatedConnection;
}): Promise<
  | { ok: IssuedCredentialData }
  | { err: "auth_failed_issuer" | "issuer_api_error" }
> => {
  const issuerIdentityRes = await authenticateForIssuer({
    authenticatedConnection,
    issuerOrigin,
  });

  if ("err" in issuerIdentityRes) {
    return { err: issuerIdentityRes.err };
  }

  const issuerIdentity = issuerIdentityRes.ok;

  const args = {
    signedIdAlias: issuerAliasCredential,
    credentialSpec,
    identity: issuerIdentity,
  };

  const preparedCredential = await vcIssuer.prepareCredential(args);

  if (preparedCredential === "error") {
    return { err: "issuer_api_error" };
  }

  const issuedCredential = await vcIssuer.getCredential({
    ...args,
    preparedCredential,
    identity: issuerIdentity,
  });

  if (issuedCredential === "error") {
    return { err: "issuer_api_error" };
  }

  return { ok: issuedCredential };
};

// Perform an authentication (delegation, etc) of the user to the issuer
// so that we can contact the issuer authenticated as the user
const authenticateForIssuer = async ({
  authenticatedConnection,
  issuerOrigin,
}: {
  authenticatedConnection: AuthenticatedConnection;
  issuerOrigin: string;
}): Promise<{ ok: DelegationIdentity } | { err: "auth_failed_issuer" }> => {
  // This is basically a copy-paste of what we have in the authentication flow

  const tempIdentity: ECDSAKeyIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const delegation = await fetchDelegation({
    connection: authenticatedConnection,
    derivationOrigin: issuerOrigin,
    publicKey: new Uint8Array(tempIdentity.getPublicKey().toDer()),
    maxTimeToLive: BigInt(5 * 60 * 1000_000_000) /* 5 minutes */,
  });

  if ("error" in delegation) {
    console.error("Could not fetch delegation");
    return { err: "auth_failed_issuer" };
  }

  const [userKey, parsed_signed_delegation] = delegation;
  const degs = {
    delegation: new Delegation(
      parsed_signed_delegation.delegation.pubkey,
      parsed_signed_delegation.delegation.expiration,
      parsed_signed_delegation.delegation.targets,
    ),
    signature: parsed_signed_delegation.signature,
  };
  const delegations = DelegationChain.fromDelegations(
    [degs],
    Uint8Array.from(userKey),
  );
  return { ok: DelegationIdentity.fromDelegation(tempIdentity, delegations) };
};

// Perform a "base64url" encoding, i.e. a URL-friendly variation of base64 encoding
const base64UrlEncode = (x: unknown): string => {
  const json = JSON.stringify(x);
  // Pretend the json is binary and use btoa (binary-to-ascii as base64) to base64 encode
  const b64 = btoa(json);
  // make it URL friendly:
  // '=': used as padding, just remove
  // '/': Base64Url as per jwt.io's playground replaces it with '_'
  // '+': Base64Url as per jwt.io's playground replaces it with '-'
  return b64.replace(/=+$/, "").replace("/", "_").replace("+", "-");
};

// Create the final presentation (to be then returned to the RP)
const createPresentation = ({
  issuerCanisterId,
  rpAliasCredential,
  issuedCredential,
}: {
  issuerCanisterId: Principal;
  rpAliasCredential: SignedIdAlias;
  issuedCredential: IssuedCredentialData;
}): VcVerifiablePresentation => {
  // The simplest JWT header, with no algorithm specified since we don't sign the payload
  const headerObj = { typ: "JWT", alg: "none" };

  const payloadObj = {
    iss: `did:icp:${issuerCanisterId.toText()}` /* JWT Issuer is set to the issuer's canister ID as per spec */,
    vp: {
      "@context": "https://www.w3.org/2018/credentials/v1",
      type: "VerifiablePresentation",
      verifiableCredential: [
        rpAliasCredential.credential_jws satisfies string,
        issuedCredential.vc_jws satisfies string,
      ] /* spec dictates first the alias creds, then the VC */,
    },
  };

  const header = base64UrlEncode(headerObj);
  const payload = base64UrlEncode(payloadObj);

  // NOTE: the JWT is not signed, as per the spec
  const signature = "";

  return { verifiablePresentation: [header, payload, signature].join(".") };
};
