import { ActorSubclass, SignIdentity } from "@dfinity/agent";
import { DelegationIdentity } from "@dfinity/identity";
import { _SERVICE } from "../../generated/internet_identity_types";

// this is only needed so that I can inject the real IIConnection into MockiiConnection (via webpack.NormalModuleReplacementPlugin in webpack.conf.js)
export class IIConnection {
  constructor(
    public identity: SignIdentity,
    public delegationIdentity: DelegationIdentity,
    public actor?: ActorSubclass<_SERVICE>
  ) {}
}

export const requestFEDelegation = undefined;
