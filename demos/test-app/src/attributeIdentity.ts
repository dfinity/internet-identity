// Local copy of `AttributeIdentity` mirroring
// https://github.com/dfinity/icp-js-core/pull/1356 plus the in-place
// mutation workaround for the call-path bug documented in
// https://github.com/dfinity/icp-js-core/pull/1356#issuecomment-4342880121.
//
// Two bugs need to be worked around in `@icp-sdk/core@5.3.1`:
//
// 1. `AttributesIdentity` (plural) injects `sender_info` into every
//    outgoing request — including `read_state` polls. The IC's
//    representation-independent hash for `read_state` does not
//    include `sender_info`, so the signature ends up over a hash the
//    IC won't reproduce; every poll fails with "Invalid basic
//    signature". PR #1356 fixes this by skipping the injection for
//    `read_state`.
//
// 2. `HttpAgent.call` computes `requestId = requestIdOf(submit)`
//    *before* `id.transformRequest` runs. When transformRequest
//    returns a new body containing `sender_info`, the wire body and
//    the polling key diverge: the IC stores the result under
//    `hash(body + sender_info)` while the agent polls for
//    `hash(body)`, so the agent loops on `processing` forever. The
//    workaround (used by `mo:identity-attributes` demos until the
//    upstream fix lands) is to mutate `request.body` in place rather
//    than building a new body. The outer `submit` reference inside
//    `HttpAgent.call` then sees `sender_info` too and `requestIdOf`
//    ends up over the same bytes the IC sees.
import {
  Endpoint,
  type HttpAgentRequest,
  type Identity,
} from "@icp-sdk/core/agent";
import type { Principal } from "@icp-sdk/core/principal";

export interface SignedAttributes {
  data: Uint8Array;
  signature: Uint8Array;
}

export interface AttributeIdentitySigner {
  canisterId: Principal;
}

export interface AttributeIdentityOptions {
  inner: Identity;
  attributes: SignedAttributes;
  signer: AttributeIdentitySigner;
}

export class AttributeIdentity implements Identity {
  readonly #inner: Identity;
  readonly #attributes: SignedAttributes;
  readonly #signer: AttributeIdentitySigner;

  constructor(options: AttributeIdentityOptions) {
    this.#inner = options.inner;
    this.#attributes = options.attributes;
    this.#signer = options.signer;
  }

  getPrincipal(): Principal {
    return this.#inner.getPrincipal();
  }

  transformRequest(request: HttpAgentRequest): Promise<unknown> {
    if (request.endpoint === Endpoint.ReadState) {
      return this.#inner.transformRequest(request);
    }
    // Mutate `request.body` in place — see comment at top of file.
    (request.body as Record<string, unknown>).sender_info = {
      signer: this.#signer.canisterId.toUint8Array(),
      info: this.#attributes.data,
      sig: this.#attributes.signature,
    };
    return this.#inner.transformRequest(request);
  }
}
