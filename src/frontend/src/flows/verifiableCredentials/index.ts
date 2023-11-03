import { SignedIdAlias } from "$generated/internet_identity_types";
import {
  CredentialSpec,
  IssuedCredentialData,
} from "$generated/vc_issuer_types";
import { authenticateBox } from "$src/components/authenticateBox";
import { withLoader } from "$src/components/loader";
import { showSpinner } from "$src/components/spinner";
import { toast } from "$src/components/toast";
import { getDapps } from "$src/flows/dappsExplorer/dapps";
import { authnTemplateManage } from "$src/flows/manage";
import { I18n } from "$src/i18n";
import { AuthenticatedConnection, Connection } from "$src/utils/iiConnection";
import { isNullish } from "@dfinity/utils";
import { base64url } from "jose";
import { allow } from "./allow";
import { VcVerifiablePresentation, vcProtocol } from "./postMessageInterface";
import { VcIssuer } from "./vcIssuer";

const dapps = getDapps();

// XXX the VC flow currently only supports the happy path
const giveUp = async (message?: string): Promise<never> => {
  console.error(message);
  toast.error("Error was encountered, giving up: " + message);
  return await new Promise((_) => {
    /* halt forever */
  });
};

// The "verifiable credentials" approval flow
export const vcFlow = async ({ connection }: { connection: Connection }) => {
  await vcProtocol({
    /* Show some spinners while we wait for more data or for an action to complete */
    onProgress: (x) => {
      if (x === "waiting") {
        return showSpinner({
          message: "Waiting for info",
        });
      }

      if (x === "verifying") {
        return showSpinner({
          message: "Verifying",
        });
      }
      x satisfies never;
    },

    /* How the credentials are actually verified */
    verifyCredentials: async ({
      request: {
        credentialSubject: givenP_RP,
        issuer: { origin: issuerOrigin, canisterId: issuerCanisterId_ },
        credentialSpec,
      },
      rpOrigin,
    }) => {
      // Go through the login flow, potentially creating an anchor.
      const { connection: authenticatedConnection } = await authenticateBox({
        connection,
        i18n: new I18n(),
        templates: authnTemplateManage({ dapps }),
      });

      // Compute the user's principal on the RP and ensure it matches what the RP sent us
      const computedP_RP = await authenticatedConnection.getPrincipal({
        origin: rpOrigin,
      });

      const pAliasPending = getAliasCredentials({
        rpOrigin,
        issuerOrigin,
        authenticatedConnection,
      });
      if (computedP_RP.compareTo(givenP_RP) !== "eq") {
        return giveUp(
          [
            "bad principals",
            computedP_RP.toString(),
            givenP_RP.toString(),
          ].join(", ")
        );
      }

      // Ask user to confirm the verification of credentials
      const allowed = await allow({
        relyingOrigin: rpOrigin,
        providerOrigin: issuerOrigin,
      });
      if (allowed === "canceled") {
        return giveUp("canceled");
      }
      allowed satisfies "allowed";

      // Grab the credentials from the issuer
      const [issuedCredential, pAlias, issuerCanisterId] = await withLoader(
        async () => {
          const issuerCanisterId =
            issuerCanisterId_?.toText() ??
            (await lookupCanister({
              origin: issuerOrigin,
            }));
          const pAlias = await pAliasPending;

          const issuedCredential = await issueCredential({
            issuerCanisterId,
            issuerAliasCredential: pAlias.issuerAliasCredential,
            credentialSpec,
          });
          return [issuedCredential, pAlias, issuerCanisterId];
        }
      );

      // Create the presentation and return it to the RP
      return createPresentation({
        issuerCanisterId,
        rpAliasCredential: pAlias.rpAliasCredential,
        issuedCredential,
      });
    },
  });
};

const lookupCanister = async ({
  origin,
}: {
  origin: string;
}): Promise<string> => {
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
    throw new Error(`Response wasn't A-OK ${response}`);
  }

  const canisterId = response.headers.get("x-ic-canister-id");

  if (isNullish(canisterId)) {
    throw new Error(`Canister ID header was not set ${response.headers}`);
  }

  return canisterId;
};

const getAliasCredentials = async ({
  authenticatedConnection,
  issuerOrigin,
  rpOrigin,
}: {
  issuerOrigin: string;
  rpOrigin: string;
  authenticatedConnection: AuthenticatedConnection;
}): Promise<{
  rpAliasCredential: SignedIdAlias;
  issuerAliasCredential: SignedIdAlias;
}> => {
  const preparedIdAlias = await authenticatedConnection.prepareIdAlias({
    issuerOrigin,
    rpOrigin,
  });

  if ("error" in preparedIdAlias) {
    return giveUp("Could not prepare alias");
  }

  const result = await authenticatedConnection.getIdAlias({
    preparedIdAlias,
    issuerOrigin,
    rpOrigin,
  });

  if ("error" in result) {
    return giveUp("Could not get alias");
  }

  const {
    rp_id_alias_credential: rpAliasCredential,
    issuer_id_alias_credential: issuerAliasCredential,
  } = result;

  return { rpAliasCredential, issuerAliasCredential };
};

const issueCredential = async ({
  issuerCanisterId,
  issuerAliasCredential,
  credentialSpec,
}: {
  issuerCanisterId: string;
  issuerAliasCredential: SignedIdAlias;
  credentialSpec: CredentialSpec;
}): Promise<IssuedCredentialData> => {
  const vcIssuer = new VcIssuer(issuerCanisterId);

  const args = {
    signedIdAlias: issuerAliasCredential,
    credentialSpec,
  };

  const preparedCredential = await vcIssuer.prepareCredential(args);

  if ("error" in preparedCredential) {
    return giveUp("Could not prepare credential");
  }

  const issuedCredential = await vcIssuer.getCredential({
    ...args,
    preparedCredential,
  });

  if ("error" in issuedCredential) {
    return giveUp("Could not issue credential");
  }

  return issuedCredential;
};

const createPresentation = ({
  issuerCanisterId,
  rpAliasCredential,
  issuedCredential,
}: {
  issuerCanisterId: string;
  rpAliasCredential: SignedIdAlias;
  issuedCredential: IssuedCredentialData;
}): VcVerifiablePresentation["result"] => {
  // The simplest JWT header, with no algorithm specified since we don't sign the payload
  const headerObj = { typ: "JWT", alg: "none" };

  const payloadObj = {
    iss: `did:icp:${issuerCanisterId}` /* JWT Issuer is set to the issuer's canister ID as per spec */,
    vp: {
      "@context": "https://www.w3.org/2018/credentials/v1",
      type: "VerifiablePresentation",
      verifiableCredential: [
        rpAliasCredential.credential_jws satisfies string,
        issuedCredential.vc_jws satisfies string,
      ] /* spec dictates first the alias creds, then the VC */,
    },
  };

  const header = base64url.encode(JSON.stringify(headerObj));
  const payload = base64url.encode(JSON.stringify(payloadObj));
  const signature = "";

  return { verifiablePresentation: [header, payload, signature].join(".") };
};
