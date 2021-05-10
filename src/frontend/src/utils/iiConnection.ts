import {
  Actor,
  ActorSubclass,
  BinaryBlob,
  blobFromUint8Array,
  derBlobFromBlob,
  DerEncodedBlob,
  HttpAgent,
  SignIdentity,
} from "@dfinity/agent";
import internet_identity_idl from "../../generated/internet_identity_idl";
import _SERVICE, {
  PublicKey,
  SessionKey,
  CredentialId,
  UserNumber,
  FrontendHostname,
  Timestamp,
  DeviceData,
  ProofOfWork,
  RegisterResponse,
  GetDelegationResponse,
} from "../../generated/internet_identity_types";
import {
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
  WebAuthnIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/agent";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";
import { hasOwnProperty } from "./utils";

// eslint-disable-next-line
const canisterId: string = process.env.CANISTER_ID!;
export const canisterIdPrincipal: Principal = Principal.fromText(canisterId);
export const baseActor = Actor.createActor<_SERVICE>(internet_identity_idl, {
  agent: new HttpAgent({}),
  canisterId,
});

export type ApiResult = LoginResult | RegisterResult;
export type LoginResult = LoginSuccess | UnknownUser | AuthFail | ApiError;
export type RegisterResult =
  | LoginSuccess
  | AuthFail
  | ApiError
  | RegisterNoSpace;

type LoginSuccess = {
  kind: "loginSuccess";
  connection: IIConnection;
  userNumber: bigint;
};
type UnknownUser = { kind: "unknownUser"; userNumber: bigint };
type AuthFail = { kind: "authFail"; error: Error };
type ApiError = { kind: "apiError"; error: Error };
type RegisterNoSpace = { kind: "registerNoSpace" };

export class IIConnection {
  protected constructor(
    public identity: WebAuthnIdentity,
    public delegationIdentity: DelegationIdentity,
    public actor?: ActorSubclass<_SERVICE>
  ) {}

  static async register(
    identity: WebAuthnIdentity,
    alias: string,
    pow: ProofOfWork
  ): Promise<RegisterResult> {
    let delegationIdentity: DelegationIdentity;
    try {
      delegationIdentity = await requestFEDelegation(identity);
    } catch (error) {
      return { kind: "authFail", error };
    }

    const agent = new HttpAgent({ identity: delegationIdentity });
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId: canisterId,
    });
    const credential_id = Array.from(identity.rawId);
    const pubkey = Array.from(identity.getPublicKey().toDer());

    console.log(
      `register(DeviceData { alias=${alias}, pubkey=${pubkey}, credential_id=${credential_id} }, ProofOfWork { timestamp=${pow.timestamp}, nonce=${pow.nonce})`
    );
    let registerResponse: RegisterResponse;
    try {
      registerResponse = await actor.register(
        {
          alias,
          pubkey,
          credential_id: [credential_id],
        },
        pow
      );
    } catch (error) {
      return { kind: "apiError", error };
    }

    if (hasOwnProperty(registerResponse, "canister_full")) {
      return { kind: "registerNoSpace" };
    } else if (hasOwnProperty(registerResponse, "registered")) {
      const userNumber = registerResponse["registered"].user_number;
      console.log(`registered user number ${userNumber}`);
      return {
        kind: "loginSuccess",
        connection: new IIConnection(identity, delegationIdentity, actor),
        userNumber,
      };
    } else {
      console.error("unexpected register response", registerResponse);
      throw Error("unexpected register response");
    }
  }

  static async login(userNumber: bigint): Promise<LoginResult> {
    let devices: DeviceData[];
    try {
      devices = await baseActor.lookup(userNumber);
    } catch (e) {
      return {
        kind: "apiError",
        error: e,
      };
    }

    if (devices.length === 0) {
      return { kind: "unknownUser", userNumber };
    }

    const multiIdent = MultiWebAuthnIdentity.fromCredentials(
      devices.flatMap((device) =>
        device.credential_id.map((credentialId: CredentialId) => ({
          pubkey: derBlobFromBlob(
            blobFromUint8Array(Buffer.from(device.pubkey))
          ),
          credentialId: blobFromUint8Array(Buffer.from(credentialId)),
        }))
      )
    );
    let delegationIdentity: DelegationIdentity;
    try {
      delegationIdentity = await requestFEDelegation(multiIdent);
    } catch (e) {
      return { kind: "authFail", error: e };
    }

    const agent = new HttpAgent({ identity: delegationIdentity });
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId: canisterId,
    });

    return {
      kind: "loginSuccess",
      userNumber,
      connection: new IIConnection(
        // eslint-disable-next-line
        multiIdent._actualIdentity!,
        delegationIdentity,
        actor
      ),
    };
  }

  static async lookup(userNumber: UserNumber): Promise<DeviceData[]> {
    return baseActor.lookup(userNumber);
  }

  // Create an actor representing the backend
  async getActor(): Promise<ActorSubclass<_SERVICE>> {
    for (const { delegation } of this.delegationIdentity.getDelegation()
      .delegations) {
      // prettier-ignore
      if (+new Date(Number(delegation.expiration / BigInt(1000000))) <= +Date.now()) {
        this.actor = undefined;
        break;
      }
    }

    if (this.actor === undefined) {
      // Create our actor with a DelegationIdentity to avoid re-prompting auth
      this.delegationIdentity = await requestFEDelegation(this.identity);

      const agent = new HttpAgent({ identity: this.delegationIdentity });
      this.actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
        agent,
        canisterId,
      });
    }

    return this.actor;
  }

  add = async (
    userNumber: UserNumber,
    alias: string,
    newPublicKey: DerEncodedBlob,
    credentialId?: BinaryBlob
  ): Promise<void> => {
    const actor = await this.getActor();
    return await actor.add(userNumber, {
      alias,
      pubkey: Array.from(newPublicKey),
      credential_id: credentialId ? [Array.from(credentialId)] : [],
    });
  };

  remove = async (
    userNumber: UserNumber,
    publicKey: PublicKey
  ): Promise<void> => {
    const actor = await this.getActor();
    await actor.remove(userNumber, publicKey);
  };

  prepareDelegation = async (
    userNumber: UserNumber,
    hostname: FrontendHostname,
    sessionKey: SessionKey
  ): Promise<[PublicKey, bigint]> => {
    console.log(
      `prepare_delegation(user: ${userNumber}, hostname: ${hostname}, session_key: ${sessionKey})`
    );
    const actor = await this.getActor();
    return await actor.prepare_delegation(userNumber, hostname, sessionKey, []);
  };

  getDelegation = async (
    userNumber: UserNumber,
    hostname: FrontendHostname,
    sessionKey: SessionKey,
    timestamp: Timestamp
  ): Promise<GetDelegationResponse> => {
    console.log(
      `get_delegation(user: ${userNumber}, hostname: ${hostname}, session_key: ${sessionKey}, timestamp: ${timestamp})`
    );
    const actor = await this.getActor();
    return await actor.get_delegation(
      userNumber,
      hostname,
      sessionKey,
      timestamp
    );
  };
}

const requestFEDelegation = async (
  identity: SignIdentity
): Promise<DelegationIdentity> => {
  const sessionKey = Ed25519KeyIdentity.generate();
  const tenMinutesInMsec = 10 * 1000 * 60;
  // Here the security device is used. Besides creating new keys, this is the only place.
  const chain = await DelegationChain.create(
    identity,
    sessionKey.getPublicKey(),
    new Date(Date.now() + tenMinutesInMsec),
    {
      targets: [Principal.from(canisterId)],
    }
  );
  return DelegationIdentity.fromDelegation(sessionKey, chain);
};
