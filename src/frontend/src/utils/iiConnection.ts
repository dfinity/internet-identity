/**
 * This module contains everything related to connecting to the canister.
 */
import {
  Actor,
  ActorSubclass,
  DerEncodedPublicKey,
  HttpAgent,
  SignIdentity,
} from "@dfinity/agent";
import { idlFactory as internet_identity_idl } from "../../generated/internet_identity_idl";
import {
  _SERVICE,
  AddTentativeDeviceResponse,
  Challenge,
  ChallengeResult,
  CredentialId,
  DeviceData,
  DeviceKey,
  FrontendHostname,
  GetDelegationResponse,
  IdentityAnchorInfo,
  KeyType,
  PublicKey,
  Purpose,
  RegisterResponse,
  SessionKey,
  Timestamp,
  UserNumber,
  VerifyTentativeDeviceResponse,
} from "../../generated/internet_identity_types";
import {
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/principal";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";
import { unreachable } from "./utils";
import * as tweetnacl from "tweetnacl";
import { fromMnemonicWithoutValidation } from "../crypto/ed25519";
import { features } from "../features";

/*
 * A (dummy) identity that always uses the same keypair. The secret key is
 * generated with a 32-byte \NUL seed.
 * This identity must not be used in production.
 */
export class DummyIdentity
  extends Ed25519KeyIdentity
  implements IdentifiableIdentity
{
  public rawId: ArrayBuffer;

  public constructor() {
    const key = Ed25519KeyIdentity.generate(new Uint8Array(32));

    const { secretKey, publicKey } = key.getKeyPair();
    super(publicKey, secretKey);

    // A dummy rawId
    this.rawId = new Uint8Array(32);
  }

  public getAuthenticatorAttachment(): undefined {
    return undefined;
  }
}

export const IC_DERIVATION_PATH = [44, 223, 0, 0, 0];

export type ApiResult = LoginResult | RegisterResult;
export type LoginResult =
  | LoginSuccess
  | UnknownUser
  | AuthFail
  | ApiError
  | SeedPhraseFail;
export type RegisterResult =
  | LoginSuccess
  | AuthFail
  | ApiError
  | RegisterNoSpace
  | BadChallenge;

type LoginSuccess = {
  kind: "loginSuccess";
  connection: AuthenticatedConnection;
  userNumber: bigint;
};

type BadChallenge = { kind: "badChallenge" };
type UnknownUser = { kind: "unknownUser"; userNumber: bigint };
type AuthFail = { kind: "authFail"; error: Error };
type ApiError = { kind: "apiError"; error: Error };
type RegisterNoSpace = { kind: "registerNoSpace" };
type SeedPhraseFail = { kind: "seedPhraseFail" };

export type { ChallengeResult } from "../../generated/internet_identity_types";

export interface IdentifiableIdentity extends SignIdentity {
  rawId: ArrayBuffer;
}

export class Connection {
  public constructor(readonly canisterId: string) {}

  register = async ({
    identity,
    tempIdentity,
    alias,
    challengeResult,
  }: {
    identity: IdentifiableIdentity;
    tempIdentity: SignIdentity;
    alias: string;
    challengeResult: ChallengeResult;
  }): Promise<RegisterResult> => {
    let delegationIdentity: DelegationIdentity;
    try {
      delegationIdentity = await this.requestFEDelegation(tempIdentity);
    } catch (error: unknown) {
      if (error instanceof Error) {
        return { kind: "authFail", error };
      } else {
        return {
          kind: "authFail",
          error: new Error("Unknown error when requesting delegation"),
        };
      }
    }

    const actor = await this.createActor(delegationIdentity);
    const credential_id = Array.from(new Uint8Array(identity.rawId));
    const pubkey = Array.from(new Uint8Array(identity.getPublicKey().toDer()));

    let registerResponse: RegisterResponse;
    try {
      registerResponse = await actor.register(
        {
          alias,
          pubkey,
          credential_id: [credential_id],
          key_type: { unknown: null },
          purpose: { authentication: null },
          protection: { unprotected: null },
        },
        challengeResult,
        tempIdentity.getPrincipal()
      );
    } catch (error: unknown) {
      if (error instanceof Error) {
        return { kind: "apiError", error };
      } else {
        return {
          kind: "apiError",
          error: new Error("Unknown error when registering"),
        };
      }
    }

    if ("canister_full" in registerResponse) {
      return { kind: "registerNoSpace" };
    } else if ("registered" in registerResponse) {
      const userNumber = registerResponse.registered.user_number;
      console.log(`registered Identity Anchor ${userNumber}`);
      return {
        kind: "loginSuccess",
        connection: new AuthenticatedConnection(
          this.canisterId,
          identity,
          delegationIdentity,
          userNumber,
          actor
        ),
        userNumber,
      };
    } else if ("bad_challenge" in registerResponse) {
      return { kind: "badChallenge" };
    } else {
      console.error("unexpected register response", registerResponse);
      throw Error("unexpected register response");
    }
  };

  login = async (userNumber: bigint): Promise<LoginResult> => {
    let devices: DeviceData[];
    try {
      devices = await this.lookupAuthenticators(userNumber);
    } catch (e: unknown) {
      if (e instanceof Error) {
        return { kind: "apiError", error: e };
      } else {
        return {
          kind: "apiError",
          error: new Error("Unknown error when looking up authenticators"),
        };
      }
    }

    if (devices.length === 0) {
      return { kind: "unknownUser", userNumber };
    }

    return this.fromWebauthnDevices(userNumber, devices);
  };

  fromWebauthnDevices = async (
    userNumber: bigint,
    devices: DeviceData[]
  ): Promise<LoginResult> => {
    /* Recover the Identity (i.e. key pair) used when creating the anchor.
     * If "II_DUMMY_AUTH" is set, we use a dummy identity, the same identity
     * that is used in the register flow.
     */
    const identity =
      process.env.II_DUMMY_AUTH === "1"
        ? new DummyIdentity()
        : MultiWebAuthnIdentity.fromCredentials(
            devices.flatMap((device) =>
              device.credential_id.map((credentialId: CredentialId) => ({
                pubkey: derFromPubkey(device.pubkey),
                credentialId: Buffer.from(credentialId),
              }))
            )
          );
    let delegationIdentity: DelegationIdentity;
    try {
      delegationIdentity = await this.requestFEDelegation(identity);
    } catch (e: unknown) {
      if (e instanceof Error) {
        return { kind: "authFail", error: e };
      } else {
        return {
          kind: "authFail",
          error: new Error("Unknown error when requesting delegation"),
        };
      }
    }

    const actor = await this.createActor(delegationIdentity);

    const connection = new AuthenticatedConnection(
      this.canisterId,
      identity,
      delegationIdentity,
      userNumber,
      actor
    );

    const attachmentInfo = identity.getAuthenticatorAttachment();
    if (attachmentInfo !== undefined) {
      try {
        this.updateKeyTypeIfNecessary(devices, attachmentInfo, connection);
      } catch (e) {
        console.warn("Could not update key type:", e);
      }
    }

    return {
      kind: "loginSuccess",
      userNumber,
      connection,
    };
  };

  private updateKeyTypeIfNecessary(
    devices: DeviceData[],
    attachmentInfo: {
      credentialId: ArrayBuffer;
      authenticatorAttachment: AuthenticatorAttachment;
    },
    authenticatedConnection: AuthenticatedConnection
  ) {
    // Find the device based on the credential_id
    const device = devices.find((device) => {
      const credentialId = device.credential_id[0];
      if (credentialId === undefined) {
        return false;
      }
      return bufferEqual(
        Buffer.from(credentialId),
        attachmentInfo.credentialId
      );
    });

    // only update devices with key-type unknown
    if (device !== undefined && "unknown" in device.key_type) {
      switch (attachmentInfo.authenticatorAttachment) {
        case "cross-platform":
          device.key_type = { cross_platform: null };
          break;
        case "platform":
          device.key_type = { platform: null };
          break;
        default:
          unreachable(
            attachmentInfo.authenticatorAttachment,
            `unexpected authenticator attachment: ${attachmentInfo.authenticatorAttachment}`
          );
          break;
      }
      // we purposely do not await the promise as we just optimistically update
      // if it fails, no harm done
      authenticatedConnection.update(device).catch((error) => {
        console.warn(
          `unable to update device ${JSON.stringify(device)}, error: ${error}`
        );
      });
    }
  }

  fromSeedPhrase = async (
    userNumber: bigint,
    seedPhrase: string,
    expected: DeviceData
  ): Promise<LoginResult> => {
    const identity = await fromMnemonicWithoutValidation(
      seedPhrase,
      IC_DERIVATION_PATH
    );
    if (
      !bufferEqual(
        identity.getPublicKey().toDer(),
        derFromPubkey(expected.pubkey)
      )
    ) {
      return {
        kind: "seedPhraseFail",
      };
    }
    const delegationIdentity = await this.requestFEDelegation(identity);
    const actor = await this.createActor(delegationIdentity);

    return {
      kind: "loginSuccess",
      userNumber,
      connection: new AuthenticatedConnection(
        this.canisterId,
        identity,
        delegationIdentity,
        userNumber,
        actor
      ),
    };
  };

  lookupAll = async (userNumber: UserNumber): Promise<DeviceData[]> => {
    const actor = await this.createActor();
    return await actor.lookup(userNumber);
  };

  createChallenge = async (): Promise<Challenge> => {
    const actor = await this.createActor();
    const challenge = await actor.create_challenge();
    console.log("Challenge Created");
    return challenge;
  };

  lookupAuthenticators = async (
    userNumber: UserNumber
  ): Promise<DeviceData[]> => {
    const actor = await this.createActor();
    const allDevices = await actor.lookup(userNumber);
    return allDevices.filter((device) => "authentication" in device.purpose);
  };

  addTentativeDevice = async (
    userNumber: UserNumber,
    alias: string,
    keyType: KeyType,
    purpose: Purpose,
    newPublicKey: DerEncodedPublicKey,
    credentialId?: ArrayBuffer
  ): Promise<AddTentativeDeviceResponse> => {
    const actor = await this.createActor();
    return await actor.add_tentative_device(userNumber, {
      alias,
      pubkey: Array.from(new Uint8Array(newPublicKey)),
      credential_id: credentialId
        ? [Array.from(new Uint8Array(credentialId))]
        : [],
      key_type: keyType,
      purpose,
      protection: { unprotected: null },
    });
  };

  lookupRecovery = async (userNumber: UserNumber): Promise<DeviceData[]> => {
    const actor = await this.createActor();
    const allDevices = await actor.lookup(userNumber);
    return allDevices.filter((device) => "recovery" in device.purpose);
  };

  // Create an actor representing the backend
  createActor = async (
    delegationIdentity?: DelegationIdentity
  ): Promise<ActorSubclass<_SERVICE>> => {
    const agent = new HttpAgent({ identity: delegationIdentity });

    // Only fetch the root key when we're not in prod
    if (features.FETCH_ROOT_KEY) {
      await agent.fetchRootKey();
    }
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId: this.canisterId,
    });
    return actor;
  };

  requestFEDelegation = async (
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
        targets: [Principal.from(this.canisterId)],
      }
    );
    return DelegationIdentity.fromDelegation(sessionKey, chain);
  };
}

export class AuthenticatedConnection extends Connection {
  public constructor(
    public canisterId: string,
    public identity: SignIdentity,
    public delegationIdentity: DelegationIdentity,
    public userNumber: bigint,
    public actor?: ActorSubclass<_SERVICE>
  ) {
    super(canisterId);
  }
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
      this.delegationIdentity = await this.requestFEDelegation(this.identity);
      this.actor = await this.createActor(this.delegationIdentity);
    }

    return this.actor;
  }

  getAnchorInfo = async (): Promise<IdentityAnchorInfo> => {
    const actor = await this.getActor();
    return await actor.get_anchor_info(this.userNumber);
  };

  enterDeviceRegistrationMode = async (): Promise<Timestamp> => {
    const actor = await this.getActor();
    return await actor.enter_device_registration_mode(this.userNumber);
  };

  exitDeviceRegistrationMode = async (): Promise<void> => {
    const actor = await this.getActor();
    return await actor.exit_device_registration_mode(this.userNumber);
  };

  verifyTentativeDevice = async (
    pin: string
  ): Promise<VerifyTentativeDeviceResponse> => {
    const actor = await this.getActor();
    return await actor.verify_tentative_device(this.userNumber, pin);
  };

  add = async (
    alias: string,
    keyType: KeyType,
    purpose: Purpose,
    newPublicKey: DerEncodedPublicKey,
    protection: DeviceData["protection"],
    credentialId?: ArrayBuffer
  ): Promise<void> => {
    const actor = await this.getActor();
    return await actor.add(this.userNumber, {
      alias,
      pubkey: Array.from(new Uint8Array(newPublicKey)),
      credential_id: credentialId
        ? [Array.from(new Uint8Array(credentialId))]
        : [],
      key_type: keyType,
      purpose,
      protection,
    });
  };

  update = async (device: DeviceData): Promise<void> => {
    const actor = await this.getActor();
    return await actor.update(this.userNumber, device.pubkey, device);
  };

  remove = async (publicKey: PublicKey): Promise<void> => {
    const actor = await this.getActor();
    await actor.remove(this.userNumber, publicKey);
  };

  prepareDelegation = async (
    hostname: FrontendHostname,
    sessionKey: SessionKey,
    maxTimeToLive?: bigint
  ): Promise<[PublicKey, bigint]> => {
    console.log(
      `prepare_delegation(user: ${this.userNumber}, hostname: ${hostname}, session_key: ${sessionKey})`
    );
    const actor = await this.getActor();
    return await actor.prepare_delegation(
      this.userNumber,
      hostname,
      sessionKey,
      maxTimeToLive !== undefined ? [maxTimeToLive] : []
    );
  };

  getDelegation = async (
    hostname: FrontendHostname,
    sessionKey: SessionKey,
    timestamp: Timestamp
  ): Promise<GetDelegationResponse> => {
    console.log(
      `get_delegation(user: ${this.userNumber}, hostname: ${hostname}, session_key: ${sessionKey}, timestamp: ${timestamp})`
    );
    const actor = await this.getActor();
    return await actor.get_delegation(
      this.userNumber,
      hostname,
      sessionKey,
      timestamp
    );
  };
}

// The options sent to the browser when creating the credentials.
// Credentials (key pair) creation is signed with a private key that is unique per device
// model, as an "attestation" that the credentials were created with a FIDO
// device. In II we discard this attestation because we only care about the key
// pair that was created and that we use later. Discarding the attestation
// means we do not have to care about attestation checking security concerns
// like setting a server-generated, random challenge.
//
// Algorithm -7, ECDSA_WITH_SHA256, is specified. The reason is that the
// generated (ECDSA) key pair is used later directly to sign messages to the
// IC -- the "assertion" -- so we must use a signing algorithm supported by the
// IC:
//  * https://smartcontracts.org/docs/interface-spec/index.html#signatures
//
// For more information on attestation vs assertion (credentials.create vs
// credentials.get), see
//  * https://developer.mozilla.org/en-US/docs/Web/API/Web_Authentication_API/Attestation_and_Assertion
export const creationOptions = (
  exclude: DeviceData[] = [],
  authenticatorAttachment?: AuthenticatorAttachment
): PublicKeyCredentialCreationOptions => {
  return {
    authenticatorSelection: {
      userVerification: "preferred",
      authenticatorAttachment,
    },
    excludeCredentials: exclude.flatMap((device) =>
      device.credential_id.length === 0
        ? []
        : {
            id: new Uint8Array(device.credential_id[0]),
            type: "public-key",
          }
    ),
    challenge: Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)),
    pubKeyCredParams: [
      {
        type: "public-key",
        // alg: PubKeyCoseAlgo.ECDSA_WITH_SHA256
        alg: -7,
      },
      {
        type: "public-key",
        // alg: PubKeyCoseAlgo.RSA_WITH_SHA256
        alg: -257,
      },
    ],
    rp: {
      name: "Internet Identity Service",
    },
    user: {
      id: tweetnacl.randomBytes(16),
      name: "Internet Identity",
      displayName: "Internet Identity",
    },
  };
};

const derFromPubkey = (pubkey: DeviceKey): DerEncodedPublicKey =>
  new Uint8Array(pubkey).buffer as DerEncodedPublicKey;

export const bufferEqual = (buf1: ArrayBuffer, buf2: ArrayBuffer): boolean => {
  if (buf1.byteLength != buf2.byteLength) return false;
  const dv1 = new Int8Array(buf1);
  const dv2 = new Int8Array(buf2);
  for (let i = 0; i != buf1.byteLength; i++) {
    if (dv1[i] != dv2[i]) return false;
  }
  return true;
};
