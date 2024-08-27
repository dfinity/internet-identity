/**
 * This module contains everything related to connecting to the canister.
 */
import { idlFactory as internet_identity_idl } from "$generated/internet_identity_idl";
import {
  AddTentativeDeviceResponse,
  AnchorCredentials,
  Challenge,
  ChallengeResult,
  CredentialId,
  DeviceData,
  DeviceKey,
  FrontendHostname,
  GetDelegationResponse,
  IdAliasCredentials,
  IdentityAnchorInfo,
  IdentityInfo,
  IdentityInfoError,
  IdentityMetadataReplaceError,
  KeyType,
  MetadataMapV2,
  PreparedIdAlias,
  PublicKey,
  Purpose,
  RegisterResponse,
  SessionKey,
  Timestamp,
  UserNumber,
  VerifyTentativeDeviceResponse,
  WebAuthnCredential,
  _SERVICE,
} from "$generated/internet_identity_types";
import { fromMnemonicWithoutValidation } from "$src/crypto/ed25519";
import { features } from "$src/features";
import {
  IdentityMetadata,
  IdentityMetadataRepository,
} from "$src/repositories/identityMetadata";
import { diagnosticInfo, unknownToString } from "$src/utils/utils";
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
  ECDSAKeyIdentity,
  Ed25519KeyIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/principal";
import { isNullish, nonNullish } from "@dfinity/utils";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";
import { RecoveryDevice, isRecoveryDevice } from "./recoveryDevice";
import { isWebAuthnCancel } from "./webAuthnErrorUtils";

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
    return;
  }
}

export const IC_DERIVATION_PATH = [44, 223, 0, 0, 0];

export type LoginSuccess = {
  kind: "loginSuccess";
  connection: AuthenticatedConnection;
  userNumber: bigint;
};

export type BadChallenge = { kind: "badChallenge" };
export type BadPin = { kind: "badPin" };
export type UnknownUser = { kind: "unknownUser"; userNumber: bigint };
export type AuthFail = { kind: "authFail"; error: Error };
export type ApiError = { kind: "apiError"; error: Error };
export type RegisterNoSpace = { kind: "registerNoSpace" };
export type NoSeedPhrase = { kind: "noSeedPhrase" };
export type SeedPhraseFail = { kind: "seedPhraseFail" };
export type WebAuthnFailed = { kind: "webAuthnFailed" };

export type { ChallengeResult } from "$generated/internet_identity_types";

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
    credentialId,
    keyType,
    alias,
    challengeResult,
  }: {
    identity: SignIdentity;
    tempIdentity: SignIdentity;
    credentialId?: CredentialId;
    keyType: KeyType;
    alias: string;
    challengeResult: ChallengeResult;
  }): Promise<
    LoginSuccess | ApiError | AuthFail | RegisterNoSpace | BadChallenge
  > => {
    const delegationIdentity = await this.requestFEDelegation(tempIdentity);
    const actor = await this.createActor(delegationIdentity);
    const pubkey = Array.from(new Uint8Array(identity.getPublicKey().toDer()));

    let registerResponse: RegisterResponse;
    try {
      registerResponse = await actor.register(
        {
          alias,
          pubkey,
          credential_id: credentialId ? [credentialId] : [],
          key_type: keyType,
          purpose: { authentication: null },
          protection: { unprotected: null },
          origin: readDeviceOrigin(),
          metadata: [],
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
      console.log(`registered Internet Identity ${userNumber}`);
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

  login = async (
    userNumber: bigint
  ): Promise<
    LoginSuccess | AuthFail | WebAuthnFailed | UnknownUser | ApiError
  > => {
    let devices: Omit<DeviceData, "alias">[];
    try {
      devices = await this.lookupAuthenticators(userNumber);
    } catch (e: unknown) {
      const errObj =
        e instanceof Error
          ? e
          : new Error("Unknown error when looking up authenticators");
      return { kind: "apiError", error: errObj };
    }

    if (devices.length === 0) {
      return { kind: "unknownUser", userNumber };
    }

    return this.fromWebauthnCredentials(
      userNumber,
      devices.flatMap(({ credential_id, pubkey }) => {
        return credential_id.length === 0
          ? []
          : [{ credential_id: credential_id[0], pubkey }];
      })
    );
  };

  fromWebauthnCredentials = async (
    userNumber: bigint,
    credentials: WebAuthnCredential[]
  ): Promise<LoginSuccess | WebAuthnFailed | AuthFail> => {
    /* Recover the Identity (i.e. key pair) used when creating the anchor.
     * If the "DUMMY_AUTH" feature is set, we use a dummy identity, the same identity
     * that is used in the register flow.
     */
    const identity = features.DUMMY_AUTH
      ? new DummyIdentity()
      : MultiWebAuthnIdentity.fromCredentials(
          credentials.map(({ credential_id, pubkey }) => ({
            pubkey: derFromPubkey(pubkey),
            credentialId: Buffer.from(credential_id),
          }))
        );
    let delegationIdentity: DelegationIdentity;

    // Here we expect a webauth exception if the user canceled the webauthn prompt (triggered by
    // "sign" inside the webauthn identity), and if so bubble it up
    try {
      delegationIdentity = await this.requestFEDelegation(identity);
    } catch (e: unknown) {
      if (isWebAuthnCancel(e)) {
        return { kind: "webAuthnFailed" };
      }

      throw new Error(
        `Failed to authenticate using passkey: ${unknownToString(
          e,
          "unknown error"
        )}, ${await diagnosticInfo()}`
      );
    }

    const actor = await this.createActor(delegationIdentity);

    const connection = new AuthenticatedConnection(
      this.canisterId,
      identity,
      delegationIdentity,
      userNumber,
      actor
    );

    return {
      kind: "loginSuccess",
      userNumber,
      connection,
    };
  };
  fromIdentity = async (
    userNumber: bigint,
    identity: SignIdentity
  ): Promise<LoginSuccess> => {
    const delegationIdentity = await this.requestFEDelegation(identity);
    const actor = await this.createActor(delegationIdentity);

    const connection = new AuthenticatedConnection(
      this.canisterId,
      identity,
      delegationIdentity,
      userNumber,
      actor
    );
    return {
      kind: "loginSuccess",
      userNumber,
      connection,
    };
  };

  fromSeedPhrase = async (
    userNumber: bigint,
    seedPhrase: string
  ): Promise<LoginSuccess | NoSeedPhrase | SeedPhraseFail> => {
    const pubkeys = (await this.lookupCredentials(userNumber)).recovery_phrases;
    if (pubkeys.length === 0) {
      return {
        kind: "noSeedPhrase",
      };
    }

    const identity = await fromMnemonicWithoutValidation(
      seedPhrase,
      IC_DERIVATION_PATH
    );
    if (
      !pubkeys.some((pubkey) =>
        bufferEqual(identity.getPublicKey().toDer(), derFromPubkey(pubkey))
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

  lookupCredentials = async (
    userNumber: UserNumber
  ): Promise<AnchorCredentials> => {
    const actor = await this.createActor();
    return await actor.get_anchor_credentials(userNumber);
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
      origin: isNullish(window?.origin) ? [] : [window.origin],
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
    const sessionKey = await ECDSAKeyIdentity.generate({ extractable: false });
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
  private metadataRepository: IdentityMetadataRepository;
  public constructor(
    public canisterId: string,
    public identity: SignIdentity,
    public delegationIdentity: DelegationIdentity,
    public userNumber: bigint,
    public actor?: ActorSubclass<_SERVICE>
  ) {
    super(canisterId);
    const metadataGetter = async () => {
      const response = await this.getIdentityInfo();
      if ("Ok" in response) {
        return response.Ok.metadata;
      }
      throw new Error("Error fetching metadata");
    };
    const metadataSetter = async (metadata: MetadataMapV2) => {
      const response = await this.setIdentityMetadata(metadata);
      if ("Ok" in response) {
        return;
      }
      throw new Error("Error updating metadata");
    };
    this.metadataRepository = IdentityMetadataRepository.init({
      getter: metadataGetter,
      setter: metadataSetter,
    });
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

    if (isNullish(this.actor)) {
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

  getPrincipal = async ({
    origin: origin_,
  }: {
    origin: string;
  }): Promise<Principal> => {
    const origin = remapToLegacyDomain(origin_);

    const actor = await this.getActor();
    return await actor.get_principal(this.userNumber, origin);
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
      metadata: [],
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

  private getIdentityInfo = async (): Promise<
    { Ok: IdentityInfo } | { Err: IdentityInfoError }
  > => {
    const actor = await this.getActor();
    return await actor.identity_info(this.userNumber);
  };

  private setIdentityMetadata = async (
    metadata: MetadataMapV2
  ): Promise<{ Ok: null } | { Err: IdentityMetadataReplaceError }> => {
    const actor = await this.getActor();
    return await actor.identity_metadata_replace(this.userNumber, metadata);
  };

  getIdentityMetadata = (): Promise<IdentityMetadata | undefined> => {
    return this.metadataRepository.getMetadata();
  };

  updateIdentityMetadata = (
    partialMetadata: Partial<IdentityMetadata>
  ): Promise<void> => {
    return this.metadataRepository.updateMetadata(partialMetadata);
  };

  commitMetadata = async (): Promise<boolean> => {
    return await this.metadataRepository.commitMetadata();
  };

  prepareDelegation = async (
    origin_: FrontendHostname,
    sessionKey: SessionKey,
    maxTimeToLive?: bigint
  ): Promise<[PublicKey, bigint] | { error: unknown }> => {
    try {
      const origin = remapToLegacyDomain(origin_);
      console.log(
        `prepare_delegation(user: ${this.userNumber}, origin: ${origin}, session_key: ${sessionKey})`
      );
      const actor = await this.getActor();
      return await actor.prepare_delegation(
        this.userNumber,
        origin,
        sessionKey,
        nonNullish(maxTimeToLive) ? [maxTimeToLive] : []
      );
    } catch (e: unknown) {
      console.error(e);
      return { error: e };
    }
  };

  getDelegation = async (
    origin_: FrontendHostname,
    sessionKey: SessionKey,
    timestamp: Timestamp
  ): Promise<GetDelegationResponse | { error: unknown }> => {
    try {
      const origin = remapToLegacyDomain(origin_);
      console.log(
        `get_delegation(user: ${this.userNumber}, origin: ${origin}, session_key: ${sessionKey}, timestamp: ${timestamp})`
      );
      const actor = await this.getActor();
      return await actor.get_delegation(
        this.userNumber,
        origin,
        sessionKey,
        timestamp
      );
    } catch (e: unknown) {
      console.error(e);
      return { error: e };
    }
  };

  prepareIdAlias = async ({
    issuerOrigin: issuerOrigin_,
    rpOrigin: rpOrigin_,
  }: {
    issuerOrigin: string;
    rpOrigin: string;
  }): Promise<
    | PreparedIdAlias
    | { error: "internal_error" }
    | { error: "authentication_failed" }
  > => {
    const issuerOrigin = remapToLegacyDomain(issuerOrigin_);
    const rpOrigin = remapToLegacyDomain(rpOrigin_);
    const actor = await this.getActor();
    const userNumber = this.userNumber;
    const result = await actor.prepare_id_alias({
      issuer: issuerOrigin,
      relying_party: rpOrigin,
      identity_number: userNumber,
    });

    if (isNullish(result)) {
      console.error("Canister did not send a response");
      return { error: "internal_error" };
    }

    if ("Ok" in result) {
      return result.Ok;
    }

    if (!("Err" in result)) {
      console.error(
        "Expected property 'Ok' or 'Err', got: ",
        JSON.stringify(result)
      );
      return { error: "internal_error" };
    }

    const err = result.Err;
    if ("Unauthorized" in err) {
      return { error: "authentication_failed" };
    }

    console.error("Unknown error", err);
    return { error: "internal_error" };
  };

  getIdAlias = async ({
    preparedIdAlias,
    issuerOrigin: issuerOrigin_,
    rpOrigin: rpOrigin_,
  }: {
    preparedIdAlias: PreparedIdAlias;
    issuerOrigin: string;
    rpOrigin: string;
  }): Promise<
    | IdAliasCredentials
    | { error: "internal_error" }
    | { error: "authentication_failed" }
  > => {
    const issuerOrigin = remapToLegacyDomain(issuerOrigin_);
    const rpOrigin = remapToLegacyDomain(rpOrigin_);
    const actor = await this.getActor();
    const userNumber = this.userNumber;

    const result = await actor.get_id_alias({
      issuer: issuerOrigin,
      relying_party: rpOrigin,
      identity_number: userNumber,
      ...preparedIdAlias,
    });

    if (isNullish(result)) {
      console.error("Canister did not send a response");
      return { error: "internal_error" };
    }

    if ("Ok" in result) {
      return result.Ok;
    }

    if (!("Err" in result)) {
      console.error(
        "Expected property 'Ok' or 'Err', got: ",
        JSON.stringify(result)
      );
      return { error: "internal_error" };
    }

    const err = result.Err;
    if ("NoSuchCredentials" in err) {
      console.error(["No credentials", err.NoSuchCredentials].join(": "));
      return { error: "internal_error" };
    }
    if ("Unauthorized" in err) {
      return { error: "authentication_failed" };
    }

    console.error("Unknown error", err);
    return { error: "internal_error" };
  };
}

// Reads the "origin" used to infer what domain a FIDO device is available on.
// The canister only allow for 50 characters, so for long domains we don't attach an origin
// (those long domains are most likely a testnet with URL like <canister id>.large03.testnet.dfinity.network, and we basically only care about identity.ic0.app & identity.internetcomputer.org).
//
// The return type is odd but that's what our didc version expects.
export const readDeviceOrigin = (): [] | [string] => {
  if (isNullish(window?.origin) || window.origin.length > 50) {
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
    challenge: window.crypto.getRandomValues(new Uint8Array(16)),
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
      id: window.crypto.getRandomValues(new Uint8Array(16)),
      name: "Internet Identity",
      displayName: "Internet Identity",
    },
  };
};

// In order to give dapps a stable principal regardless whether they use the legacy (ic0.app) or the new domain (icp0.io)
// we map back the derivation origin to the ic0.app domain.
const remapToLegacyDomain = (origin: string): string => {
  const ORIGIN_MAPPING_REGEX =
    /^https:\/\/(?<subdomain>[\w-]+(?:\.raw)?)\.icp0\.io$/;
  const match = origin.match(ORIGIN_MAPPING_REGEX);
  const subdomain = match?.groups?.subdomain;
  if (nonNullish(subdomain)) {
    return `https://${subdomain}.ic0.app`;
  } else {
    return origin;
  }
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

// Infer the host for the IC's HTTP api. II lives on a custom domain that may be different
// from the domain where the api is served (agent-js otherwise infers the IC's HTTP URL from
// the current window location)
export const inferHost = (): string => {
  // The domain used for the http api
  const IC_API_DOMAIN = "icp-api.io";

  const location = window?.location;
  if (isNullish(location)) {
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
