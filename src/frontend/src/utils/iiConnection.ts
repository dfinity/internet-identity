/**
 * This module contains everything related to connecting to the canister.
 */
import { idlFactory as internet_identity_idl } from "$generated/internet_identity_idl";
import {
  _SERVICE,
  AddTentativeDeviceResponse,
  AnchorCredentials,
  AuthnMethodData,
  DeviceData,
  DeviceKey,
  FrontendHostname,
  GetDelegationResponse,
  IdAliasCredentials,
  IdentityAnchorInfo,
  IdentityInfo,
  IdentityInfoError,
  IdentityMetadataReplaceError,
  InternetIdentityInit,
  JWT,
  KeyType,
  MetadataMapV2,
  PreparedIdAlias,
  PublicKey,
  Purpose,
  RegistrationFlowNextStep,
  Salt,
  SessionKey,
  SignedDelegation,
  Timestamp,
  UserNumber,
  VerifyTentativeDeviceResponse,
} from "$generated/internet_identity_types";
import { withLoader } from "$src/components/loader";
import { toast } from "$src/components/toast";
import { fromMnemonicWithoutValidation } from "$src/crypto/ed25519";
import { DOMAIN_COMPATIBILITY, HARDWARE_KEY_TEST } from "$src/featureFlags";
import { features } from "$src/features";
import {
  IdentityMetadata,
  IdentityMetadataRepository,
} from "$src/repositories/identityMetadata";
import {
  CanisterError,
  diagnosticInfo,
  unknownToString,
} from "$src/utils/utils";
import {
  Actor,
  ActorSubclass,
  DerEncodedPublicKey,
  HttpAgent,
  Signature,
  SignIdentity,
} from "@dfinity/agent";
import {
  Delegation,
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
  Ed25519KeyIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/principal";
import { isNullish, nonNullish } from "@dfinity/utils";
import {
  convertToValidCredentialData,
  CredentialData,
} from "./credential-devices";
import { findWebAuthnFlows, WebAuthnFlow } from "./findWebAuthnFlows";
import { relatedDomains } from "./findWebAuthnRpId";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";
import { isRecoveryDevice, RecoveryDevice } from "./recoveryDevice";
import { supportsWebauthRoR } from "./userAgent";
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
  showAddCurrentDevice: boolean;
};

export type RegFlowNextStep =
  | { step: "checkCaptcha"; captcha_png_base64: string }
  | { step: "finish" };
export type RegistrationFlowStepSuccess = {
  kind: "registrationFlowStepSuccess";
  nextStep: RegFlowNextStep;
};

export type BadPin = { kind: "badPin" };
export type UnknownUser = { kind: "unknownUser"; userNumber: bigint };
export type UnexpectedCall = {
  kind: "unexpectedCall";
  nextStep: RegFlowNextStep;
};
export type NoRegistrationFlow = { kind: "noRegistrationFlow" };
export type WrongCaptchaSolution = {
  kind: "wrongCaptchaSolution";
  new_captcha_png_base64: string;
};
export type AuthFail = { kind: "authFail"; error: Error };
export type InvalidCaller = { kind: "invalidCaller" };
export type RateLimitExceeded = { kind: "rateLimitExceeded" };
export type AlreadyInProgress = { kind: "alreadyInProgress" };
export type ApiError = { kind: "apiError"; error: Error };
export type RegisterNoSpace = { kind: "registerNoSpace" };
export type NoSeedPhrase = { kind: "noSeedPhrase" };
export type SeedPhraseFail = { kind: "seedPhraseFail" };
export type WebAuthnFailed = { kind: "webAuthnFailed" };
export type PossiblyWrongWebAuthnFlow = { kind: "possiblyWrongWebAuthnFlow" };
// The user has PIN identity but in another domain and II can't access it.
export type PinUserOtherDomain = { kind: "pinUserOtherDomain" };
export type InvalidAuthnMethod = {
  kind: "invalidAuthnMethod";
  message: string;
};

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
  private configPromise: Promise<InternetIdentityInit> | undefined;
  private webAuthFlows:
    | { flows: WebAuthnFlow[]; currentIndex: number }
    | undefined;

  public constructor(
    readonly canisterId: string,
    // Used for testing purposes
    readonly overrideActor?: ActorSubclass<_SERVICE>
  ) {}

  identity_registration_start = async ({
    tempIdentity,
  }: {
    tempIdentity: SignIdentity;
  }): Promise<
    | RegistrationFlowStepSuccess
    | ApiError
    | InvalidCaller
    | AlreadyInProgress
    | RateLimitExceeded
  > => {
    const actor = await this.createActor(tempIdentity);

    let startResponse;
    try {
      startResponse = await actor.identity_registration_start();
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

    if ("Ok" in startResponse) {
      return {
        kind: "registrationFlowStepSuccess",
        nextStep: mapRegFlowNextStep(startResponse.Ok.next_step),
      };
    }

    if ("Err" in startResponse) {
      const err = startResponse.Err;
      if ("InvalidCaller" in err) {
        return { kind: "invalidCaller" };
      }
      if ("AlreadyInProgress" in err) {
        return { kind: "alreadyInProgress" };
      }
      if ("RateLimitExceeded" in err) {
        return { kind: "rateLimitExceeded" };
      }
    }
    console.error(
      "unexpected identity_registration_start response",
      startResponse
    );
    throw Error("unexpected identity_registration_start response");
  };

  check_captcha = async ({
    tempIdentity,
    captchaSolution,
  }: {
    tempIdentity: SignIdentity;
    captchaSolution: string;
  }): Promise<
    | RegistrationFlowStepSuccess
    | ApiError
    | NoRegistrationFlow
    | UnexpectedCall
    | WrongCaptchaSolution
  > => {
    const actor = await this.createActor(tempIdentity);

    let captchaResponse;
    try {
      captchaResponse = await actor.check_captcha({
        solution: captchaSolution,
      });
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

    if ("Ok" in captchaResponse) {
      const nextStep = captchaResponse.Ok.next_step;
      if ("Finish" in nextStep) {
        return {
          kind: "registrationFlowStepSuccess",
          nextStep: { step: "finish" },
        };
      }
      console.error(
        "unexpected next step in check_captcha response",
        captchaResponse
      );
      throw Error("unexpected next step in check_captcha response");
    }

    if ("Err" in captchaResponse) {
      const err = captchaResponse.Err;
      if ("WrongSolution" in err) {
        return {
          kind: "wrongCaptchaSolution",
          new_captcha_png_base64: err.WrongSolution.new_captcha_png_base64,
        };
      }
      if ("UnexpectedCall" in err) {
        return {
          kind: "unexpectedCall",
          nextStep: mapRegFlowNextStep(err.UnexpectedCall.next_step),
        };
      }
      if ("NoRegistrationFlow" in err) {
        return { kind: "noRegistrationFlow" };
      }
    }
    console.error("unexpected check_captcha response", captchaResponse);
    throw Error("unexpected check_captcha response");
  };

  identity_registration_finish = async ({
    tempIdentity,
    identity,
    authnMethod,
  }: {
    tempIdentity: SignIdentity;
    identity: SignIdentity;
    authnMethod: AuthnMethodData;
  }): Promise<
    | LoginSuccess
    | ApiError
    | NoRegistrationFlow
    | UnexpectedCall
    | RegisterNoSpace
    | InvalidAuthnMethod
  > => {
    const delegationIdentity = await this.requestFEDelegation(tempIdentity);
    const actor = await this.createActor(delegationIdentity);

    let finishResponse;
    try {
      finishResponse = await actor.identity_registration_finish({
        authn_method: authnMethod,
      });
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

    if ("Ok" in finishResponse) {
      const userNumber = finishResponse.Ok.identity_number;
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
        showAddCurrentDevice: false,
      };
    }

    if ("Err" in finishResponse) {
      const err = finishResponse.Err;
      if ("InvalidAuthnMethod" in err) {
        return {
          kind: "invalidAuthnMethod",
          message: err.InvalidAuthnMethod,
        };
      }
      if ("UnexpectedCall" in err) {
        return {
          kind: "unexpectedCall",
          nextStep: mapRegFlowNextStep(err.UnexpectedCall.next_step),
        };
      }
      if ("NoRegistrationFlow" in err) {
        return { kind: "noRegistrationFlow" };
      }
      if ("IdentityLimitReached" in err) {
        return { kind: "registerNoSpace" };
      }
      if ("StorageError" in err) {
        // this is unrecoverable, so we can just map it to a generic API error
        return {
          kind: "apiError",
          error: new Error("StorageError: " + err.StorageError),
        };
      }
    }
    console.error("unexpected check_captcha response", finishResponse);
    throw Error("unexpected check_captcha response");
  };

  login = async (
    userNumber: bigint
  ): Promise<
    | LoginSuccess
    | AuthFail
    | WebAuthnFailed
    | PossiblyWrongWebAuthnFlow
    | PinUserOtherDomain
    | UnknownUser
    | ApiError
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

    let webAuthnAuthenticators = devices.filter(
      ({ key_type }) => !("browser_storage_key" in key_type)
    );

    // If we reach this point, it's because no PIN identity was found.
    // Therefore, it's because it was created in another domain.
    if (webAuthnAuthenticators.length === 0) {
      return { kind: "pinUserOtherDomain" };
    }

    if (HARDWARE_KEY_TEST.isEnabled()) {
      webAuthnAuthenticators = webAuthnAuthenticators.filter(
        (device) =>
          Object.keys(device.key_type)[0] === "unknown" ||
          Object.keys(device.key_type)[0] === "cross_platform"
      );
    }

    return this.fromWebauthnCredentials(
      userNumber,
      webAuthnAuthenticators
        .map(convertToValidCredentialData)
        .filter(nonNullish)
    );
  };

  fromWebauthnCredentials = async (
    userNumber: bigint,
    credentials: CredentialData[]
  ): Promise<
    LoginSuccess | WebAuthnFailed | PossiblyWrongWebAuthnFlow | AuthFail
  > => {
    if (isNullish(this.webAuthFlows) && DOMAIN_COMPATIBILITY.isEnabled()) {
      const flows = findWebAuthnFlows({
        supportsRor: supportsWebauthRoR(window.navigator.userAgent),
        devices: credentials,
        currentOrigin: window.location.origin,
        relatedOrigins: relatedDomains(),
      });
      this.webAuthFlows = {
        flows,
        currentIndex: 0,
      };
    }
    const flowsLength = this.webAuthFlows?.flows.length ?? 0;
    // We reached the last flow. Start from the beginning.
    // This might happen if the user cancelled manually in the flow that would have been successful.
    if (this.webAuthFlows?.currentIndex === flowsLength) {
      this.webAuthFlows.currentIndex = 0;
    }
    const currentFlow = nonNullish(this.webAuthFlows)
      ? this.webAuthFlows.flows[this.webAuthFlows.currentIndex]
      : undefined;

    /* Recover the Identity (i.e. key pair) used when creating the anchor.
     * If the "DUMMY_AUTH" feature is set, we use a dummy identity, the same identity
     * that is used in the register flow.
     */
    const identity = features.DUMMY_AUTH
      ? new DummyIdentity()
      : // Passing all the credentials doesn't hurt and it could help in case an `origin` was wrongly set in the backend.
        MultiWebAuthnIdentity.fromCredentials(
          credentials,
          currentFlow?.rpId,
          currentFlow?.useIframe ?? false
        );
    let delegationIdentity: DelegationIdentity;

    // Here we expect a webauth exception if the user canceled the webauthn prompt (triggered by
    // "sign" inside the webauthn identity), and if so bubble it up
    try {
      delegationIdentity = await this.requestFEDelegation(identity);
    } catch (e: unknown) {
      if (isWebAuthnCancel(e)) {
        // We only want to show a special error if the user might have to choose different web auth flow.
        if (nonNullish(this.webAuthFlows) && flowsLength > 1) {
          // Increase the index to try the next flow.
          this.webAuthFlows = {
            flows: this.webAuthFlows.flows,
            currentIndex: this.webAuthFlows.currentIndex + 1,
          };
          return { kind: "possiblyWrongWebAuthnFlow" };
        }
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

    // If the index is more than 0, it's because the first one failed.
    // We should offer to add the current device to the current origin.
    const showAddCurrentDevice = (this.webAuthFlows?.currentIndex ?? 0) > 0;

    return {
      kind: "loginSuccess",
      userNumber,
      connection,
      showAddCurrentDevice,
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
      showAddCurrentDevice: false,
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
      showAddCurrentDevice: false,
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
    identity?: SignIdentity
  ): Promise<ActorSubclass<_SERVICE>> => {
    if (this.overrideActor !== undefined) {
      return this.overrideActor;
    }
    const agent = await HttpAgent.create({
      identity,
      host: inferHost(),
      // Only fetch the root key when we're not in prod
      shouldFetchRootKey: features.FETCH_ROOT_KEY,
    });

    return Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId: this.canisterId,
    });
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

  /**
   * Get previously fetched config, else fetch it
   * TODO: Discuss with prodsec if this should stay a query or should be update,
   *       alternatively the config can also be set directly in the html head.
   */
  getConfig = (): Promise<InternetIdentityInit> => {
    this.configPromise =
      this.configPromise ?? this.createActor().then((actor) => actor.config());
    return this.configPromise;
  };

  fromJwt = async (
    jwt: JWT,
    salt: Salt,
    sessionIdentity: SignIdentity
  ): Promise<AuthenticatedConnection> => {
    const retryGetJwtDelegation = async (
      jwt: JWT,
      salt: Salt,
      sessionKey: SessionKey,
      expiration: bigint,
      actor: ActorSubclass<_SERVICE>,
      maxRetries = 5
    ): Promise<SignedDelegation> => {
      for (let i = 0; i < maxRetries; i++) {
        // Linear backoff
        await new Promise((resolve) => {
          setInterval(resolve, 1000 * i);
        });
        const res = await actor.openid_get_delegation(
          jwt,
          salt,
          sessionKey,
          expiration
        );
        if ("Err" in res) {
          const errorKeys = Object.keys(res.Err);
          const errorMessage =
            errorKeys.length > 0 ? errorKeys[0] : "unknown error";
          toast.error("Error while fetching delegation: " + errorMessage);
          continue;
        }

        return res.Ok;
      }
      throw new Error(
        `Failed to retrieve a delegation after ${maxRetries} retries.`
      );
    };

    let actor = await this.createActor(sessionIdentity);
    const sessionKey = new Uint8Array(sessionIdentity.getPublicKey().toDer());

    const prepareDelegationResponse = await actor.openid_prepare_delegation(
      jwt,
      salt,
      sessionKey,
      window.location.hostname
    );

    if ("Err" in prepareDelegationResponse)
      throw new CanisterError(prepareDelegationResponse.Err);

    const { anchor_number, expiration, user_key } =
      prepareDelegationResponse.Ok;

    const signedDelegation = await withLoader(() =>
      retryGetJwtDelegation(jwt, salt, sessionKey, expiration, actor)
    );

    const transformedDelegation = {
      delegation: new Delegation(
        new Uint8Array(signedDelegation.delegation.pubkey),
        signedDelegation.delegation.expiration,
        signedDelegation.delegation.targets?.[0]?.map((p) =>
          Principal.from(p)
        ) ?? undefined
      ),
      signature: new Uint8Array(signedDelegation.signature).buffer as Signature,
    };

    const chain = DelegationChain.fromDelegations(
      [transformedDelegation],
      new Uint8Array(user_key)
    );

    const jwtSignedIdentity = DelegationIdentity.fromDelegation(
      sessionIdentity,
      chain
    );

    actor = await this.createActor(jwtSignedIdentity);

    return new AuthenticatedConnection(
      this.canisterId,
      sessionIdentity,
      jwtSignedIdentity,
      anchor_number,
      actor
    );
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
    return actor.get_anchor_info(this.userNumber);
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
    origin: string | undefined,
    credentialId?: ArrayBuffer
  ): Promise<void> => {
    const actor = await this.getActor();
    // The canister only allow for 50 characters, so for long domains we don't attach an origin
    // (those long domains are most likely a testnet with URL like <canister id>.large03.testnet.dfinity.network, and we basically only care about identity.ic0.app & identity.internetcomputer.org).
    const sanitizedOrigin =
      nonNullish(origin) && origin.length <= 50 ? origin : undefined;
    return await actor.add(this.userNumber, {
      alias,
      pubkey: Array.from(new Uint8Array(newPublicKey)),
      credential_id: credentialId
        ? [Array.from(new Uint8Array(credentialId))]
        : [],
      key_type: keyType,
      purpose,
      protection,
      origin: sanitizedOrigin === undefined ? [] : [sanitizedOrigin],
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

  updateIdentityMetadata = (partialMetadata: Partial<IdentityMetadata>) => {
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

  addOpenIdCredential = async (jwt: JWT, salt: Salt): Promise<void> => {
    const actor = await this.getActor();
    const res = await actor.openid_credential_add(this.userNumber, jwt, salt);
    if ("Err" in res) throw new CanisterError(res.Err);
  };

  removeOpenIdCredential = async (iss: string, sub: string): Promise<void> => {
    const actor = await this.getActor();
    const res = await actor.openid_credential_remove(this.userNumber, [
      iss,
      sub,
    ]);
    if ("Err" in res) throw new CanisterError(res.Err);
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
  exclude: Omit<DeviceData, "alias">[] = [],
  authenticatorAttachment?: AuthenticatorAttachment,
  rpId?: string
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
      id: rpId,
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

const mapRegFlowNextStep = (
  step: RegistrationFlowNextStep
): RegFlowNextStep => {
  if ("Finish" in step) {
    return { step: "finish" };
  }
  step satisfies { CheckCaptcha: { captcha_png_base64: string } };
  return {
    step: "checkCaptcha",
    captcha_png_base64: step.CheckCaptcha.captcha_png_base64,
  };
};
