import type { SignIdentity, Signature } from "@icp-sdk/core/agent";
import {
  Delegation,
  DelegationChain,
  DelegationIdentity,
  SignedDelegation,
} from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import { Signer } from "@slide-computer/signer";
import { PostMessageTransport } from "@slide-computer/signer-web";

// The type of response from II as per the spec
interface AuthResponseSuccess {
  kind: "authorize-client-success";
  delegations: {
    delegation: {
      pubkey: Uint8Array;
      expiration: bigint;
      targets?: Principal[];
    };
    signature: Uint8Array;
  }[];
  userPublicKey: Uint8Array;
  authnMethod: "pin" | "passkey" | "recovery";
}

export interface CertifiedAttribute {
  value: Uint8Array;
  signature: Uint8Array;
  expiration: bigint;
}

// Perform a sign in to II using parameters set in this app
export const authWithII = async ({
  url: url_,
  maxTimeToLive,
  allowPinAuthentication,
  derivationOrigin,
  sessionIdentity,
  autoSelectionPrincipal,
  useIcrc25,
  requestAttributes,
}: {
  url: string;
  maxTimeToLive?: bigint;
  allowPinAuthentication?: boolean;
  derivationOrigin?: string;
  autoSelectionPrincipal?: string;
  sessionIdentity: SignIdentity;
  useIcrc25?: boolean;
  requestAttributes?: string[];
}): Promise<{
  identity: DelegationIdentity;
  authnMethod: string;
  certifiedAttributes?: Record<string, CertifiedAttribute>;
}> => {
  // Authenticate with signer-js instead if we use the ICRC-25 protocol
  if (useIcrc25) {
    const transport = new PostMessageTransport({ url: url_ });
    const signer = new Signer({ transport, derivationOrigin });
    const attributesRequestId = window.crypto.randomUUID();
    const attributesPromise =
      requestAttributes !== undefined && requestAttributes.length > 0
        ? signer
            .sendRequest({
              jsonrpc: "2.0",
              method: "ii_attributes",
              id: attributesRequestId,
              params: {
                attributes: requestAttributes,
              },
            })
            .then((response) => {
              if (
                !("result" in response) ||
                typeof response.result !== "object" ||
                response.result === null ||
                !("attributes" in response.result)
              ) {
                throw new Error("Attributes response is missing result");
              }
              return Object.fromEntries(
                Object.entries(response.result.attributes).map(
                  ([key, { value, signature, expiration }]) =>
                    [
                      key,
                      {
                        // @ts-ignore
                        value: Uint8Array.fromBase64(value),
                        // @ts-ignore
                        signature: Uint8Array.fromBase64(signature),
                        expiration: BigInt(expiration),
                      },
                    ] as [string, CertifiedAttribute],
                ),
              );
            })
        : Promise.resolve({});
    const delegationPromise = signer.delegation({
      maxTimeToLive,
      publicKey: new Uint8Array(sessionIdentity.getPublicKey().toDer()),
    });
    const [delegation, certifiedAttributes] = await Promise.all([
      delegationPromise,
      attributesPromise,
    ]);
    return {
      identity: DelegationIdentity.fromDelegation(
        sessionIdentity,
        // We need to cast the delegation from signer-js to avoid a TS issue because one type is imported from cjs and another esm:
        // Types of property 'delegations' are incompatible.
        delegation as unknown as DelegationChain,
      ),
      authnMethod: "passkey",
      certifiedAttributes,
    };
  }

  // Figure out the II URL to use
  const iiUrl = new URL(url_);
  iiUrl.hash = "#authorize";

  // Open an II window and kickstart the flow
  const win = window.open(iiUrl, "ii-window");
  if (win === null) {
    throw new Error(`Could not open window for '${iiUrl}'`);
  }

  // Wait for II to say it's ready
  const evnt = await new Promise<MessageEvent>((resolve) => {
    const readyHandler = (e: MessageEvent) => {
      if (e.origin !== iiUrl.origin) {
        // Ignore messages from other origins (e.g. from a metamask extension)
        return;
      }
      window.removeEventListener("message", readyHandler);
      resolve(e);
    };
    window.addEventListener("message", readyHandler);
  });

  if (evnt.data.kind !== "authorize-ready") {
    throw new Error("Bad message from II window: " + JSON.stringify(evnt));
  }

  // Send the request to II
  const sessionPublicKey: Uint8Array = new Uint8Array(
    sessionIdentity.getPublicKey().toDer(),
  );

  const request = {
    kind: "authorize-client",
    sessionPublicKey,
    maxTimeToLive,
    derivationOrigin,
    allowPinAuthentication,
    autoSelectionPrincipal,
  };

  win.postMessage(request, iiUrl.origin);

  // Wait for the II response and update the local state
  const response = await new Promise<MessageEvent>((resolve) => {
    const responseHandler = (e: MessageEvent) => {
      if (e.origin !== iiUrl.origin) {
        // Ignore messages from other origins (e.g. from a metamask extension)
        return;
      }
      window.removeEventListener("message", responseHandler);
      win.close();
      resolve(e);
    };
    window.addEventListener("message", responseHandler);
  });

  const message = response.data;
  if (message.kind !== "authorize-client-success") {
    throw new Error("Bad reply: " + JSON.stringify(message));
  }

  const identity = identityFromResponse({
    response: message as AuthResponseSuccess,
    sessionIdentity,
  });

  return { identity, authnMethod: message.authnMethod };
};

// Read delegations the delegations from the response
const identityFromResponse = ({
  sessionIdentity,
  response,
}: {
  sessionIdentity: SignIdentity;
  response: AuthResponseSuccess;
}): DelegationIdentity => {
  const delegations = response.delegations.map(extractDelegation);

  const delegationChain = DelegationChain.fromDelegations(
    delegations,
    response.userPublicKey,
  );

  const identity = DelegationIdentity.fromDelegation(
    sessionIdentity,
    delegationChain,
  );

  return identity;
};

// Infer the type of an array's elements
type ElementOf<Arr> = Arr extends readonly (infer ElementOf)[]
  ? ElementOf
  : "argument is not an array";

export const extractDelegation = (
  signedDelegation: ElementOf<AuthResponseSuccess["delegations"]>,
): SignedDelegation => ({
  delegation: new Delegation(
    signedDelegation.delegation.pubkey,
    signedDelegation.delegation.expiration,
    signedDelegation.delegation.targets,
  ),
  signature: signedDelegation.signature
    .buffer as Signature /* brand type for agent-js */,
});
