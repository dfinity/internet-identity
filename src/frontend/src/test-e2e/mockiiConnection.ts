import { LoginResult } from "../utils/iiConnection";
import { ActorSubclass, SignIdentity } from "@dfinity/agent";
import { DelegationIdentity } from "@dfinity/identity";
import { _SERVICE } from "../../generated/internet_identity_types";
import {
  IIConnection as OriginalIIConnection,
  requestFEDelegation as originalRequestFEDelegation
} from "./originaliiConnection";
import { fromMnemonicWithoutValidation } from "../crypto/ed25519";

// this mock IIConnection replaces the real IIConnection if process.env.MOCK=true (via webpack.NormalModuleReplacementPlugin in webpack.conf.js)
export class IIConnection {

  constructor(public identity: SignIdentity,
              public delegationIdentity: DelegationIdentity,
              public actor?: ActorSubclass<_SERVICE>) {
  }

  static async login(userNumber: bigint): Promise<LoginResult> {
    console.log('----------- mocked login ------------');

    const testPhrase =
      "script strategy fat bonus minimum elegant art hire vital palace combine vague proud exercise arrow copy media aim sleep soul energy crane amazing rely";

    const testIdentity = await fromMnemonicWithoutValidation(testPhrase);

    // @ts-ignore
    const delegationIdentity = await originalRequestFEDelegation(testIdentity);
    // @ts-ignore
    const actor = await OriginalIIConnection.createActor(delegationIdentity);

    return {
      kind: "loginSuccess",
      userNumber,
      // @ts-ignore
      connection: new OriginalIIConnection(
        testIdentity,
        delegationIdentity,
        actor
      ),
    };
  }
}
