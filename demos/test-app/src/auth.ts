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

export interface Icrc3Attributes {
  data: Uint8Array;
  signature: Uint8Array;
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
  useIcrc3Attributes,
  icrc3Nonce,
}: {
  url: string;
  maxTimeToLive?: bigint;
  allowPinAuthentication?: boolean;
  derivationOrigin?: string;
  autoSelectionPrincipal?: string;
  sessionIdentity: SignIdentity;
  useIcrc25?: boolean;
  requestAttributes?: string[];
  useIcrc3Attributes?: boolean;
  icrc3Nonce?: Uint8Array;
}): Promise<{
  identity: DelegationIdentity;
  authnMethod: string;
  certifiedAttributes?: Record<string, CertifiedAttribute>;
  icrc3Attributes?: Icrc3Attributes;
}> => {
  // Authenticate with signer-js instead if we use the ICRC-25 protocol
  if (useIcrc25) {
    const transport = new PostMessageTransport({ url: url_ });
    const signer = new Signer({
      transport,
      derivationOrigin,
      autoCloseTransportChannel: false,
    });
    const hasAttributes =
      requestAttributes !== undefined && requestAttributes.length > 0;

    const legacyAttributesPromise =
      hasAttributes && !useIcrc3Attributes
        ? signer
            .sendRequest({
              jsonrpc: "2.0",
              method: "ii_attributes",
              id: window.crypto.randomUUID(),
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
                        // @ts-ignore Not known in TS types yet but supported in all browsers
                        value: Uint8Array.fromBase64(value),
                        // @ts-ignore Not known in TS types yet but supported in all browsers
                        signature: Uint8Array.fromBase64(signature),
                        expiration: BigInt(expiration),
                      },
                    ] as [string, CertifiedAttribute],
                ),
              );
            })
        : Promise.resolve(undefined);

    const nonce = icrc3Nonce ?? crypto.getRandomValues(new Uint8Array(32));
    const icrc3AttributesPromise =
      hasAttributes && useIcrc3Attributes
        ? signer
            .sendRequest({
              jsonrpc: "2.0",
              method: "ii-icrc3-attributes",
              id: window.crypto.randomUUID(),
              params: {
                keys: requestAttributes,
                // @ts-ignore Not known in TS types yet but supported in all browsers
                nonce: nonce.toBase64(),
              },
            })
            .then((response) => {
              if (
                !("result" in response) ||
                typeof response.result !== "object" ||
                response.result === null ||
                !("data" in response.result) ||
                !("signature" in response.result)
              ) {
                return undefined;
              }
              return {
                // @ts-ignore Not known in TS types yet but supported in all browsers
                data: Uint8Array.fromBase64(response.result.data),
                // @ts-ignore Not known in TS types yet but supported in all browsers
                signature: Uint8Array.fromBase64(response.result.signature),
              };
            })
        : Promise.resolve(undefined);

    const delegationPromise = signer.delegation({
      maxTimeToLive,
      publicKey: new Uint8Array(sessionIdentity.getPublicKey().toDer()),
    });
    const [delegation, certifiedAttributes, icrc3Attributes] =
      await Promise.all([
        delegationPromise,
        legacyAttributesPromise,
        icrc3AttributesPromise,
      ]);
    await signer.closeChannel();
    return {
      identity: DelegationIdentity.fromDelegation(
        sessionIdentity,
        // We need to cast the delegation from signer-js to avoid a TS issue because one type is imported from cjs and another esm:
        // Types of property 'delegations' are incompatible.
        delegation as unknown as DelegationChain,
      ),
      authnMethod: "passkey",
      certifiedAttributes: certifiedAttributes ?? undefined,
      icrc3Attributes: icrc3Attributes ?? undefined,
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
