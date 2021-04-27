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
import idp_idl from "../../generated/idp_idl";
import _SERVICE, {
  PublicKey,
  SessionKey,
  CredentialId,
  UserNumber,
  FrontendHostname,
  Timestamp,
  DeviceData,
} from "../../generated/idp_types";
import {
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
  WebAuthnIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/agent";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";
import getProofOfWork from "../crypto/pow";

const canisterId: string = process.env.CANISTER_ID!;
const canisterIdPrincipal: Principal = Principal.fromText(canisterId);
export const baseActor = Actor.createActor<_SERVICE>(idp_idl, {
  agent: new HttpAgent({}),
  canisterId,
});

export class IDPActor {
  protected constructor(
    public identity: WebAuthnIdentity,
    public delegationIdentity: DelegationIdentity,
    public actor?: ActorSubclass<_SERVICE>
  ) {}

  static async register(
    identity: WebAuthnIdentity,
    alias: string,
  ): Promise<{ connection: IDPActor; userNumber: UserNumber }> {
    const delegationIdentity = await requestFEDelegation(identity);

    // Do PoW before registering.
    const now_in_ns = BigInt(Date.now()) * BigInt(1000000);
    const pow = getProofOfWork(now_in_ns, canisterIdPrincipal);

    const agent = new HttpAgent({ identity: delegationIdentity });
    const actor = Actor.createActor<_SERVICE>(idp_idl, {
      agent,
      canisterId: canisterId,
    });
    const credential_id = Array.from(identity.rawId);
    const pubkey = Array.from(identity.getPublicKey().toDer());

    console.log(`register(DeviceData { alias=${alias}, pubkey=${pubkey}, credential_id=${credential_id} }, ProofOfWork { timestamp=${pow.timestamp}, nonce=${pow.nonce})`);
    const registerResponse = await actor.register({
      alias,
      pubkey,
      credential_id: [credential_id],
    }, pow);

    if (registerResponse.hasOwnProperty('canister_full')) {
      throw Error('failed to register a user because the backend canister has no space left');
    } else if (registerResponse.hasOwnProperty('registered')) {
      let userNumber = registerResponse['registered'].user_number;
      console.log(`registered user number ${userNumber}`);
      return {
        connection: new IDPActor(identity, delegationIdentity, actor),
        userNumber,
      };
    } else {
      console.error('unexpected register response', registerResponse);
      throw Error('unexpected register response');
    }
  }

  static async login(userNumber: bigint): Promise<IDPActor> {
    const devices = await baseActor.lookup(userNumber);

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
    const delegationIdentity = await requestFEDelegation(multiIdent);

    const agent = new HttpAgent({ identity: delegationIdentity });
    const actor = Actor.createActor<_SERVICE>(idp_idl, {
      agent,
      canisterId: canisterId,
    });

    return new IDPActor(
      multiIdent._actualIdentity!!,
      delegationIdentity,
      actor
    );
  }

  static async lookup(userNumber: UserNumber): Promise<DeviceData[]> {
    return baseActor.lookup(userNumber);
  }

  // Create an actor representing the backend
  async getActor(): Promise<ActorSubclass<_SERVICE>> {
    for (const { delegation } of this.delegationIdentity.getDelegation()
      .delegations || []) {
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
      this.actor = Actor.createActor<_SERVICE>(idp_idl, {
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
  ) => {
    const actor = await this.getActor();
    return await actor.add(userNumber, {
      alias,
      pubkey: Array.from(newPublicKey),
      credential_id: credentialId ? [Array.from(credentialId)] : [],
    });
  };

  remove = async (userNumber: UserNumber, publicKey: PublicKey) => {
    const actor = await this.getActor();
    await actor.remove(userNumber, publicKey);
  };

  prepareDelegation = async (
    userNumber: UserNumber,
    hostname: FrontendHostname,
    sessionKey: SessionKey
  ) => {
    console.log(
      `prepare_delegation(user: ${userNumber}, hostname: ${hostname}, session_key: ${sessionKey})`
    );
    const actor = await this.getActor();
    return await actor.prepare_delegation(userNumber, hostname, sessionKey);
  };

  getDelegation = async (
    userNumber: UserNumber,
    hostname: FrontendHostname,
    sessionKey: SessionKey,
    timestamp: Timestamp
  ) => {
    console.log(
      `get_delegation(user: ${userNumber}, hostname: ${hostname}, session_key: ${sessionKey}, timestamp: ${timestamp})`
    );
    const actor = await this.getActor();
    return await actor.get_delegation(userNumber, hostname, sessionKey, timestamp);
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
