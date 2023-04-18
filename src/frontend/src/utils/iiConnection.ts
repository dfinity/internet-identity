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
import {
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/principal";
import * as tweetnacl from "tweetnacl";
import { idlFactory as internet_identity_idl } from "../../generated/internet_identity_idl";
import {
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
  _SERVICE,
} from "../../generated/internet_identity_types";
import { fromMnemonicWithoutValidation } from "../crypto/ed25519";
import { features } from "../features";
import { authenticatorAttachmentToKeyType } from "./authenticatorAttachment";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";
import { isRecoveryDevice, RecoveryDevice } from "./recoveryDevice";

/*
 * A (dummy) identity that always uses the same keypair. The secret key is
 * generated with a 32-byte \NUL seed.
 * This identity must not be used in production.
 */
export class DummyIdentity
  extends Ed25519KeyIdentity
  implements IIWebAuthnIdentity
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

/**
 * Interface around the agent-js WebAuthnIdentity that allows us to provide
 * alternate implementations (such as the dummy identity).
 */
export interface IIWebAuthnIdentity extends SignIdentity {
  rawId: ArrayBuffer;

  getAuthenticatorAttachment(): AuthenticatorAttachment | undefined;
}

export class Connection {
  public constructor(readonly canisterId: string) {}

  register = async ({
    identity,
    tempIdentity,
    alias,
    challengeResult,
  }: {
    identity: IIWebAuthnIdentity;
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
          key_type: authenticatorAttachmentToKeyType(
            identity.getAuthenticatorAttachment()
          ),
          purpose: { authentication: null },
          protection: { unprotected: null },
          origin: readDeviceOrigin(),
          meta_data: [],
        },
        challengeResult,
        [tempIdentity.getPrincipal()]
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
    let devices: Omit<DeviceData, "alias">[];
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
    devices: Omit<DeviceData, "alias">[]
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

    try {
      this.updateOriginIfNecessary(
        devices,
        delegationIdentity.getPublicKey().toDer(),
        connection
      );
    } catch (e) {
      console.warn("Could not update device origin:", e);
    }

    return {
      kind: "loginSuccess",
      userNumber,
      connection,
    };
  };

  private updateOriginIfNecessary(
    devices: Omit<DeviceData, "alias">[],
    pubkey: ArrayBuffer,
    authenticatedConnection: AuthenticatedConnection
  ) {
    // Find the device based on the pubkey
    const device = findDeviceByPubkey(devices, pubkey);

    // only update devices without origin information
    if (device !== undefined && device.origin.length === 0) {
      device.origin satisfies string[];
      // we purposely do not await the promise as we just optimistically update
      // if it fails, no harm done

      authenticatedConnection
        // We need to refetch the device using `getAnchorInfo` because the devices obtained by `lookup` no longer have an alias
        .getAnchorInfo()
        .then((info) => info.devices)
        .then((devices) =>
          findDeviceByPubkey(devices, Buffer.from(device.pubkey))
        )
        .then((device) => {
          if (device === undefined) {
            // this can happen if the device has been deleted between authentication and now
            throw Error("device is undefined");
          }
          device.origin = readDeviceOrigin();
          return device;
        })
        .then((device) => authenticatedConnection.update(device))
        .catch((error) => {
          console.warn(
            `unable to update device ${JSON.stringify(device)}, error: ${error}`
          );
        });
    }
  }

  fromIdentity = async (
    userNumber: bigint,
    identity: SignIdentity
  ): Promise<AuthenticatedConnection> => {
    const delegationIdentity = await this.requestFEDelegation(identity);
    const actor = await this.createActor(delegationIdentity);

    return new AuthenticatedConnection(
      this.canisterId,
      identity,
      delegationIdentity,
      userNumber,
      actor
    );
  };

  fromSeedPhrase = async (
    userNumber: bigint,
    seedPhrase: string,
    expected: Omit<DeviceData, "alias">
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

  lookupAll = async (
    userNumber: UserNumber
  ): Promise<Omit<DeviceData, "alias">[]> => {
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
  ): Promise<Omit<DeviceData, "alias">[]> => {
    const actor = await this.createActor();
    const allDevices = await actor.lookup(userNumber);
    return allDevices.filter((device) => "authentication" in device.purpose);
  };

  addTentativeDevice = async (
    userNumber: UserNumber,
    device: Omit<DeviceData, "origin">
  ): Promise<AddTentativeDeviceResponse> => {
    const actor = await this.createActor();
    return await actor.add_tentative_device(userNumber, {
      ...device,
      origin: window?.origin === undefined ? [] : [window.origin],
    });
  };

  lookupRecovery = async (
    userNumber: UserNumber
  ): Promise<RecoveryDevice[]> => {
    const actor = await this.createActor();
    // lookup blanks out the alias for privacy reasons -> omit alias from DeviceData
    const allDevices: Omit<DeviceData, "alias">[] = await actor.lookup(
      userNumber
    );
    return allDevices.filter(isRecoveryDevice);
  };

  // Create an actor representing the backend
  createActor = async (
    delegationIdentity?: DelegationIdentity
  ): Promise<ActorSubclass<_SERVICE>> => {
    const agent = new HttpAgent({
      identity: delegationIdentity,
      host: inferHost(),
    });

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
      origin: readDeviceOrigin(),
      meta_data: [],
    });
  };

  update = async (device: DeviceData): Promise<void> => {
    const actor = await this.getActor();
    return await actor.update(this.userNumber, device.pubkey, device);
  };

  replace = async (pubkey: DeviceKey, device: DeviceData): Promise<void> => {
    const actor = await this.getActor();
    return await actor.replace(this.userNumber, pubkey, device);
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

// Reads the "origin" used to infer what domain a FIDO device is available on.
// The canister only allow for 50 characters, so for long domains we don't attach an origin
// (those long domains are most likely a testnet with URL like <canister id>.large03.testnet.dfinity.network, and we basically only care about identity.ic0.app & identity.internetcomputer.org).
//
// The return type is odd but that's what our didc version expects.
export const readDeviceOrigin = (): [] | [string] => {
  if (window?.origin === undefined || window.origin.length > 50) {
    return [];
  }

  return [window.origin];
};

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
  exclude: Omit<DeviceData, "alias">[] = [],
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

function findDeviceByPubkey<T extends Omit<DeviceData, "alias">>(
  devices: T[],
  pubkey: ArrayBuffer
): T | undefined {
  return devices.find((device) => {
    return bufferEqual(Buffer.from(device.pubkey), pubkey);
  });
}

// Infer the host for the IC's HTTP api. II lives on a custom domain that may be different
// from the domain where the api is served (agent-js otherwise infers the IC's HTTP URL from
// the current window location)
export const inferHost = (): string => {
  // The domain used for the http api
  const IC_API_DOMAIN = "icp-api.io";

  const location = window?.location;
  if (location === undefined) {
    // If there is no location, then most likely this is a non-browser environment. All bets
    // are off but we return something valid just in case.
    return "https://" + IC_API_DOMAIN;
  }

  if (
    location.hostname.endsWith("icp0.io") ||
    location.hostname.endsWith("ic0.app") ||
    location.hostname.endsWith("internetcomputer.org")
  ) {
    // If this is a canister running on one of the official IC domains, then return the
    // official API endpoint
    return "https://" + IC_API_DOMAIN;
  }

  if (
    location.host === "127.0.0.1" /* typical development */ ||
    location.host ===
      "0.0.0.0" /* typical development, though no secure context (only usable with builds with WebAuthn disabled) */ ||
    location.hostname.endsWith(
      "localhost"
    ) /* local canisters from icx-proxy like rdmx6-....-foo.localhost */
  ) {
    // If this is a local deployment, then assume the api and assets are collocated
    // and use this asset (page)'s URL.
    return location.protocol + "//" + location.host;
  }

  // Otherwise assume it's a custom setup and use the host itself as API.
  return location.protocol + "//" + location.host;
};
