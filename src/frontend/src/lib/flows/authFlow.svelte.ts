import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";
import {
  authenticateWithJWT,
  authenticateWithPasskey,
  authenticateWithSession,
  authenticateWithSso,
  SsoNormalLoginRequiredError,
} from "$lib/utils/authentication";
import { frontendCanisterConfig, canisterId } from "$lib/globals";
import {
  authenticatedStore,
  authenticationStore,
} from "$lib/stores/authentication.store";
import {
  lastUsedIdentitiesStore,
  type LastUsedIdentity,
} from "$lib/stores/last-used-identities.store";
import { sessionStore } from "$lib/stores/session.store";
import { get } from "svelte/store";
import { features } from "$lib/legacy/features";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import { retryWhilePending } from "$lib/utils/openidPoll";
import {
  CheckCaptchaError,
  IdentityAnchorInfo,
  IdRegFinishError,
  IdRegStartError,
  OpenIdDelegationError,
  OpenIdConfig,
  MetadataMapV2,
} from "$lib/generated/internet_identity_types";
import {
  requestJWT,
  RequestConfig,
  decodeJWT,
  extractIssuerTemplateClaims,
  findConfig,
  selectAuthScopes,
} from "$lib/utils/openID";
import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
import { nanosToMillis } from "$lib/utils/time";

interface AuthFlowOptions {
  trackLastUsed?: boolean;
}

export type AuthMode = "signin" | "signup" | "both";

export type MethodTag = "passkey" | "openid" | "sso";

// Subset of LastUsedIdentity that the sign-in paths produce ahead of
// the user committing to it — matches the shape `addLastUsedIdentity`
// accepts (timestamp is stamped by the store on commit).
export type PendingLastUsedEntry = Pick<
  LastUsedIdentity,
  "identityNumber" | "name" | "authMethod" | "createdAtMillis"
>;

export class AuthFlow {
  #options: Required<AuthFlowOptions>;
  #view = $state<
    | "chooseMethod"
    | "setupOrUseExistingPasskey"
    | "setupNewPasskey"
    | "setupNewIdentity"
    | "signInWithSso"
    | "openIdNotConnected"
    | "openIdAlreadyLinked"
    | "confirmMethodSwitch"
  >("chooseMethod");
  #captcha = $state<{
    image: string;
    attempt: number;
    solve: (solution: string) => void;
  }>();
  #systemOverlay = $state(false);
  #name = $state<string>();
  #jwt = $state<string>();
  #configIssuer = $state<string>();
  // SSO discovery domain for a 1-click OpenID/SSO flow whose JWT is redeemed
  // through `continueWithOpenId`; `undefined` for a direct provider.
  #openIdDiscoveryDomain = $state<string>();
  #ssoJwt = $state<string>();
  #ssoDomain = $state<string>();
  #ssoName = $state<string>();
  // Dapp SSO context for the in-flight sign-in; undefined for direct providers and management SSO.
  #sso = $state<{ origin: string }>();
  #mode = $state<AuthMode>("both");
  #pendingOpenIdSignIn = $state<bigint>();
  #pendingMethodSwitch = $state<{
    previousIdentity: LastUsedIdentity;
    newMethod: MethodTag;
    signedInIdentityNumber: bigint;
    pendingLastUsedEntry?: PendingLastUsedEntry;
    providerIssuer?: string;
    providerDomain?: string;
    providerName?: string;
  }>();

  get view() {
    return this.#view;
  }

  get captcha() {
    return this.#captcha;
  }

  get systemOverlay() {
    return this.#systemOverlay;
  }

  get mode(): AuthMode {
    return this.#mode;
  }

  get configIssuer(): string | undefined {
    return this.#configIssuer;
  }

  get userName(): string | undefined {
    const jwt = this.#jwt ?? this.#ssoJwt;
    return jwt === undefined ? undefined : decodeJWT(jwt).name;
  }

  get userEmail(): string | undefined {
    const jwt = this.#jwt ?? this.#ssoJwt;
    return jwt === undefined ? undefined : decodeJWT(jwt).email;
  }

  // Display name for the parked provider (OIDC or SSO). OIDC resolves
  // via `openid_configs` by issuer; SSO falls back to discovered name
  // or domain.
  get providerName(): string | undefined {
    if (this.#configIssuer !== undefined) {
      const config = findConfig(this.#configIssuer, undefined, []);
      return config?.name ?? this.#configIssuer;
    }
    return this.#ssoName ?? this.#ssoDomain;
  }

  get pendingMethodSwitch() {
    return this.#pendingMethodSwitch;
  }

  setMode = (mode: AuthMode): void => {
    this.#mode = mode;
  };

  requestMethodSwitch = (args: {
    previousIdentity: LastUsedIdentity;
    newMethod: MethodTag;
    signedInIdentityNumber: bigint;
    pendingLastUsedEntry?: PendingLastUsedEntry;
    providerIssuer?: string;
    providerDomain?: string;
    providerName?: string;
  }): void => {
    this.#pendingMethodSwitch = args;
    this.#view = "confirmMethodSwitch";
  };

  confirmMethodSwitch = (): bigint => {
    if (this.#pendingMethodSwitch === undefined) {
      throw new Error("No pending method switch");
    }
    const { signedInIdentityNumber } = this.#pendingMethodSwitch;
    this.#pendingMethodSwitch = undefined;
    this.#view = "chooseMethod";
    return signedInIdentityNumber;
  };

  cancelMethodSwitch = (): void => {
    this.#pendingMethodSwitch = undefined;
    this.#view = "chooseMethod";
  };

  confirmOpenIdSignUp = async (): Promise<bigint | "needs-name"> => {
    if (this.#jwt !== undefined && this.#configIssuer !== undefined) {
      const { name } = decodeJWT(this.#jwt);
      if (name !== undefined) {
        return await this.completeOpenIdRegistration(name);
      }
      this.#view = "setupNewIdentity";
      return "needs-name";
    }
    if (this.#ssoJwt !== undefined && this.#ssoDomain !== undefined) {
      const { name } = decodeJWT(this.#ssoJwt);
      if (name !== undefined) {
        return await this.completeSsoRegistration(name);
      }
      this.#view = "setupNewIdentity";
      return "needs-name";
    }
    throw new Error("No pending OpenID or SSO sign-up");
  };

  confirmOpenIdSignIn = (): bigint => {
    if (this.#pendingOpenIdSignIn === undefined) {
      throw new Error("No pending OpenID sign-in");
    }
    const identityNumber = this.#pendingOpenIdSignIn;
    this.#pendingOpenIdSignIn = undefined;
    this.#jwt = undefined;
    this.#configIssuer = undefined;
    this.#openIdDiscoveryDomain = undefined;
    this.#ssoJwt = undefined;
    this.#ssoDomain = undefined;
    this.#ssoName = undefined;
    this.#sso = undefined;
    this.#view = "chooseMethod";
    return identityNumber;
  };

  cancelOpenIdDisambiguation = (): void => {
    this.#pendingOpenIdSignIn = undefined;
    this.#jwt = undefined;
    this.#configIssuer = undefined;
    this.#openIdDiscoveryDomain = undefined;
    this.#ssoJwt = undefined;
    this.#ssoDomain = undefined;
    this.#ssoName = undefined;
    this.#sso = undefined;
    this.#view = "chooseMethod";
  };

  constructor(options?: AuthFlowOptions) {
    this.#options = {
      trackLastUsed: true,
      ...options,
    };
    this.chooseMethod();
  }

  chooseMethod = (): void => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.SelectMethodScreen);
    this.#view = "chooseMethod";
  };

  setupOrUseExistingPasskey = (): void => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.ContinueWithPasskeyScreen,
    );
    this.#view = "setupOrUseExistingPasskey";
  };

  signInWithSso = (): void => {
    this.#view = "signInWithSso";
  };

  continueWithSso = async (
    ssoResult: SsoDiscoveryResult,
    mode: AuthMode = this.#mode,
    // Target dapp origin; present routes sign-in via the SSO gate path, absent uses the openid path.
    dappOrigin?: string,
  ): Promise<
    | {
        identityNumber: bigint;
        pendingLastUsedEntry?: PendingLastUsedEntry;
        type: "signIn";
      }
    | {
        name?: string;
        email?: string;
        type: "signUp";
      }
    | undefined
  > => {
    const { resolvedClientId, discovery, domain, name: ssoName } = ssoResult;
    authenticationV2Funnel.addProperties({ provider: "SSO" });
    const sso = dappOrigin !== undefined ? { origin: dappOrigin } : undefined;
    const result = await this.#openIdJwtSignIn(
      {
        clientId: resolvedClientId,
        authURL: discovery.authorization_endpoint,
        authScope: selectAuthScopes(discovery.scopes_supported).join(" "),
      },
      undefined,
      domain,
      sso,
    );
    if (result.type === "signIn") {
      const lastUsedEntry: PendingLastUsedEntry | undefined = this.#options
        .trackLastUsed
        ? {
            identityNumber: result.identityNumber,
            name: result.info.name[0],
            authMethod: {
              sso: {
                domain,
                name: ssoName,
                email: decodeJWT(result.jwt).email,
                loginHint: result.loginHint,
              },
            },
            createdAtMillis: result.info.created_at.map(nanosToMillis)[0],
          }
        : undefined;
      if (mode === "signup") {
        // The openIdAlreadyLinked dialog asks the user whether to sign
        // in with the already-linked identity. Commit eagerly here —
        // matches the existing UX: the auth succeeded, the identity is
        // tracked, and `cancelOpenIdDisambiguation` does not revert.
        if (lastUsedEntry !== undefined) {
          lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedEntry);
        }
        this.#ssoJwt = result.jwt;
        this.#ssoDomain = domain;
        this.#ssoName = ssoName;
        this.#pendingOpenIdSignIn = result.identityNumber;
        this.#view = "openIdAlreadyLinked";
        return undefined;
      }
      return {
        identityNumber: result.identityNumber,
        pendingLastUsedEntry: lastUsedEntry,
        type: "signIn",
      };
    }
    this.#ssoJwt = result.jwt;
    this.#ssoDomain = domain;
    this.#ssoName = ssoName;
    if (mode === "signin") {
      this.#view = "openIdNotConnected";
      return undefined;
    }
    return {
      name: result.suggestedName,
      email: result.email,
      type: "signUp",
    };
  };

  completeSsoRegistration = async (name: string): Promise<bigint> => {
    if (this.#ssoJwt === undefined || this.#ssoDomain === undefined) {
      throw new Error("SSO JWT or domain is missing");
    }
    authenticationV2Funnel.trigger(AuthenticationV2Events.RegisterWithOpenID);
    await this.#startRegistration();
    return this.#registerWithSso(
      this.#ssoJwt,
      name,
      this.#ssoDomain,
      this.#ssoName,
    );
  };

  // The "sign in again" dialog's action for the wizard's own gated attempt: run
  // one normal (primary-client) sign-in to bridge the identity, then replay the
  // stashed gated JWT — no fresh ceremony — so the gated sign-in resolves.
  // Returns the signed-in anchor, or undefined if there's nothing stashed / it
  // still can't resolve. Captures the gated context up front because the normal
  // sign-in below overwrites `#ssoJwt` / `#sso`.
  completeSsoNormalLoginRecovery = async (
    primaryResult: SsoDiscoveryResult,
  ): Promise<bigint | undefined> => {
    const gatedJwt = this.#ssoJwt;
    const gatedDomain = this.#ssoDomain;
    const gatedSso = this.#sso;
    const gatedName = this.#ssoName;
    if (
      gatedJwt === undefined ||
      gatedDomain === undefined ||
      gatedSso === undefined
    ) {
      return undefined;
    }
    const normal = await this.continueWithSso(primaryResult, "both");
    if (normal?.type === "signUp") {
      await this.completeSsoRegistration(
        normal.name ??
          normal.email?.split("@")[0] ??
          primaryResult.name ??
          primaryResult.domain,
      );
    }
    const { iss, aud } = decodeJWT(gatedJwt);
    const config: OpenIdConfig = {
      auth_uri: "",
      jwks_uri: "",
      logo: "",
      name: gatedName ?? gatedDomain,
      fedcm_uri: [],
      email_verification: [],
      issuer: iss,
      auth_scope: [],
      client_id: aud ?? "",
      seed_jwks: [],
    };
    const result = await this.continueWithOpenId(
      config,
      gatedJwt,
      "signin",
      gatedDomain,
      gatedSso,
    );
    return result?.type === "signIn" ? result.identityNumber : undefined;
  };

  continueWithExistingPasskey = async (): Promise<{
    identityNumber: bigint;
    pendingLastUsedEntry?: PendingLastUsedEntry;
  }> => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.UseExistingPasskey);
    const { identity, identityNumber, credentialId } =
      await authenticateWithPasskey({
        canisterId,
        session: get(sessionStore),
      });
    const authMethod = { passkey: { credentialId } };
    await authenticationStore.set({ identity, identityNumber, authMethod });
    const info =
      await get(authenticatedStore).actor.get_anchor_info(identityNumber);
    const pendingLastUsedEntry = this.#options.trackLastUsed
      ? {
          identityNumber,
          name: info.name[0],
          authMethod,
          createdAtMillis: info.created_at.map(nanosToMillis)[0],
        }
      : undefined;
    return { identityNumber, pendingLastUsedEntry };
  };

  setupNewPasskey = (): void => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.EnterNameScreen);
    this.#view = "setupNewPasskey";
  };

  setupNewIdentity = (): void => {
    this.#view = "setupNewIdentity";
  };

  submitNameAndContinue = async (
    name: string,
  ): Promise<{ type: "created"; identityNumber: bigint }> => {
    this.#name = name;
    return { type: "created", identityNumber: await this.createPasskey() };
  };

  createPasskey = async (): Promise<bigint> => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.StartWebauthnCreation,
    );
    if (this.#name === undefined) {
      throw new Error("Name is not set");
    }
    const passkeyIdentity =
      features.DUMMY_AUTH ||
      frontendCanisterConfig.dummy_auth[0]?.[0] !== undefined
        ? await DiscoverableDummyIdentity.createNew(this.#name)
        : await DiscoverablePasskeyIdentity.createNew(this.#name);
    await this.#startRegistration();
    return this.#registerWithPasskey(passkeyIdentity);
  };

  continueWithOpenId = async (
    config: OpenIdConfig,
    existingJwt?: string,
    mode: AuthMode = this.#mode,
    discoveryDomain?: string,
    // Dapp SSO context; routes the 1-click resume through the SSO gate path.
    sso?: { origin: string },
  ): Promise<
    | {
        identityNumber: bigint;
        name?: string;
        email?: string;
        pendingLastUsedEntry?: PendingLastUsedEntry;
        type: "signIn";
      }
    | {
        name?: string;
        email?: string;
        type: "signUp";
      }
    | undefined
  > => {
    authenticationV2Funnel.addProperties({ provider: config.name });
    const result = await this.#openIdJwtSignIn(
      {
        clientId: config.client_id,
        authURL: config.auth_uri,
        authScope: config.auth_scope.join(" "),
        configURL: config.fedcm_uri?.[0],
      },
      existingJwt,
      discoveryDomain,
      sso,
    );
    if (result.type === "signIn") {
      const lastUsedEntry: PendingLastUsedEntry | undefined = this.#options
        .trackLastUsed
        ? {
            identityNumber: result.identityNumber,
            name: result.info.name[0],
            // A discovery domain means the JWT was minted through on-demand
            // SSO discovery rather than a static `openid_configs` provider,
            // so track the credential as `sso` (keyed by domain). An `openid`
            // entry would send a later "last used" sign-in down the
            // static-config branch of `authLastUsedFlow`, which can't resolve
            // an SSO issuer nor pass the discovery domain to the canister.
            authMethod:
              discoveryDomain !== undefined
                ? {
                    sso: {
                      domain: discoveryDomain,
                      email: decodeJWT(result.jwt).email,
                      loginHint: result.loginHint,
                    },
                  }
                : {
                    openid: {
                      iss: result.iss,
                      sub: result.sub,
                      loginHint: result.loginHint,
                      metadata: result.info.openid_credentials[0]?.find(
                        (method) => method.iss === result.iss,
                      )?.metadata,
                    },
                  },
            createdAtMillis: result.info.created_at.map(nanosToMillis)[0],
          }
        : undefined;
      if (mode === "signup") {
        // See `continueWithSso`: the openIdAlreadyLinked path commits
        // eagerly to preserve the existing UX of that disambiguation.
        if (lastUsedEntry !== undefined) {
          lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedEntry);
        }
        this.#jwt = result.jwt;
        this.#configIssuer = config.issuer;
        this.#openIdDiscoveryDomain = discoveryDomain;
        this.#pendingOpenIdSignIn = result.identityNumber;
        this.#view = "openIdAlreadyLinked";
        return undefined;
      }
      // A caller-supplied JWT means this is the non-interactive 1-click
      // resume flow: there is no method-switch disambiguation to gate the
      // write, so AuthFlow persists the last-used entry itself rather than
      // handing it back. The interactive AuthWizard fetches the JWT itself
      // (`existingJwt === undefined`) and commits the returned entry once any
      // disambiguation clears — see `AuthWizard.commitLastUsedEntry`.
      if (existingJwt !== undefined && lastUsedEntry !== undefined) {
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedEntry);
      }
      const { name: jwtName, email } = decodeJWT(result.jwt);
      return {
        identityNumber: result.identityNumber,
        name: result.info.name[0] ?? jwtName,
        email,
        pendingLastUsedEntry:
          existingJwt === undefined ? lastUsedEntry : undefined,
        type: "signIn",
      };
    }
    this.#jwt = result.jwt;
    this.#configIssuer = config.issuer;
    this.#openIdDiscoveryDomain = discoveryDomain;
    if (mode === "signin") {
      this.#view = "openIdNotConnected";
      return undefined;
    }
    return {
      name: result.suggestedName,
      email: result.email,
      type: "signUp",
    };
  };

  // Shared core for `continueWithOpenId` / `continueWithSso`. Acquires the
  // JWT (skipped when one is supplied), authenticates with II, and either
  // returns sign-in details (with anchor info) or signals that registration
  // is needed. Knows nothing about how either caller persists LastUsedIdentity
  // — that's the caller's responsibility, so the OpenID and SSO variants
  // don't bleed into each other.
  #openIdJwtSignIn = async (
    requestConfig: RequestConfig,
    existingJwt?: string,
    discoveryDomain?: string,
    // Dapp SSO context; when set, redeems the JWT via the SSO gate path (`discoveryDomain` is always present alongside).
    sso?: { origin: string },
  ): Promise<
    | {
        type: "signIn";
        jwt: string;
        iss: string;
        sub: string;
        loginHint?: string;
        identityNumber: bigint;
        info: IdentityAnchorInfo;
      }
    | {
        type: "signUp";
        jwt: string;
        suggestedName?: string;
        email?: string;
      }
  > => {
    this.#sso = sso;
    let jwt: string | undefined = existingJwt;
    if (jwt === undefined) {
      // Two try-catch blocks to avoid double-triggering the analytics.
      try {
        this.#systemOverlay = true;
        jwt = await requestJWT(requestConfig, {
          nonce: get(sessionStore).nonce,
          mediation: "required",
        });
      } catch (error) {
        this.#view = "chooseMethod";
        throw error;
      } finally {
        this.#systemOverlay = false;
        // Moved after `requestJWT` to avoid Safari blocking the popup.
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.ContinueWithOpenID,
        );
      }
    }
    try {
      const { iss, sub, loginHint } = decodeJWT(jwt);
      const { identity, identityNumber } =
        sso !== undefined && discoveryDomain !== undefined
          ? await authenticateWithSso({
              canisterId,
              session: get(sessionStore),
              jwt,
              discoveryDomain,
              origin: sso.origin,
            })
          : await authenticateWithJWT({
              canisterId,
              session: get(sessionStore),
              jwt,
              discoveryDomain,
            });
      authenticationV2Funnel.trigger(AuthenticationV2Events.LoginWithOpenID);
      await authenticationStore.set({
        identity,
        identityNumber,
        authMethod: { openid: { iss, sub } },
      });
      const info =
        await get(authenticatedStore).actor.get_anchor_info(identityNumber);
      return {
        type: "signIn",
        jwt,
        iss,
        sub,
        loginHint,
        identityNumber,
        info,
      };
    } catch (error) {
      if (
        isCanisterError<OpenIdDelegationError>(error) &&
        error.type === "NoSuchAnchor"
      ) {
        const { name, email } = decodeJWT(jwt);
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.RegisterWithOpenID,
        );
        return { type: "signUp", jwt, suggestedName: name, email };
      }
      throw error;
    }
  };

  completeOpenIdRegistration = async (name: string): Promise<bigint> => {
    if (this.#jwt === undefined || this.#configIssuer === undefined) {
      throw new Error("JWT or config issuer is missing");
    }
    authenticationV2Funnel.trigger(AuthenticationV2Events.RegisterWithOpenID);
    await this.#startRegistration();
    return this.#registerWithOpenId(
      this.#jwt,
      name,
      this.#configIssuer,
      this.#openIdDiscoveryDomain,
    );
  };

  #solveCaptcha = (image: string, attempt = 0): Promise<void> =>
    new Promise((resolve, reject) => {
      this.#captcha = {
        image,
        attempt,
        solve: async (solution) => {
          try {
            await get(sessionStore)
              .actor.check_captcha({ solution })
              .then(throwCanisterError);
            resolve();
          } catch (error) {
            if (
              isCanisterError<CheckCaptchaError>(error) &&
              error.type === "WrongSolution"
            ) {
              const nextImage = `data:image/png;base64,${error.value(error.type).new_captcha_png_base64}`;
              this.#solveCaptcha(nextImage, attempt + 1)
                .then(resolve)
                .catch(reject);
              return;
            }
            reject(error);
          }
        },
      };
    });

  #registerWithPasskey = async (
    passkeyIdentity: DiscoverablePasskeyIdentity,
    attempts = 0,
  ): Promise<bigint> => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.RegisterWithPasskey);
    const authnMethod = passkeyAuthnMethodData({
      pubKey: passkeyIdentity.getPublicKey().toDer(),
      credentialId: passkeyIdentity.getCredentialId()!,
      authenticatorAttachment: passkeyIdentity.getAuthenticatorAttachment(),
      origin: window.location.origin,
      aaguid: passkeyIdentity.getAaguid(),
    });
    const name = passkeyIdentity.getName();
    try {
      const { identity_number: identityNumber } = await get(sessionStore)
        .actor.identity_registration_finish({
          name: name !== undefined ? [name] : [],
          authn_method: authnMethod,
        })
        .then(throwCanisterError);
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulPasskeyRegistration,
      );
      const credentialId = new Uint8Array(passkeyIdentity.getCredentialId()!);
      const identity = await authenticateWithSession({
        session: get(sessionStore),
      });
      await authenticationStore.set({
        identity,
        identityNumber,
        authMethod: { passkey: { credentialId } },
      });
      if (this.#options.trackLastUsed) {
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber,
          name: passkeyIdentity.getName(),
          authMethod: { passkey: { credentialId } },
          createdAtMillis: Date.now(),
        });
      }
      this.#captcha = undefined;
      return identityNumber;
    } catch (error) {
      if (isCanisterError<IdRegFinishError>(error)) {
        switch (error.type) {
          case "UnexpectedCall": {
            const nextStep = error.value(error.type).next_step;
            if ("CheckCaptcha" in nextStep) {
              if (attempts < 3) {
                await this.#solveCaptcha(
                  `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`,
                );
                return this.#registerWithPasskey(passkeyIdentity, attempts + 1);
              }
            }
            break;
          }
          case "NoRegistrationFlow":
            if (attempts < 3) {
              // Apparently the flow has been cleaned up, try again.
              await this.#startRegistration();
              return await this.#registerWithPasskey(
                passkeyIdentity,
                attempts + 1,
              );
            }
            break;
        }
      }
      throw error;
    }
  };

  #startRegistration = async (): Promise<void> => {
    try {
      const { next_step } = await get(sessionStore)
        .actor.identity_registration_start()
        .then(throwCanisterError);
      if ("CheckCaptcha" in next_step) {
        await this.#solveCaptcha(
          `data:image/png;base64,${next_step.CheckCaptcha.captcha_png_base64}`,
        );
      }
    } catch (error) {
      if (
        isCanisterError<IdRegStartError>(error) &&
        error.type === "AlreadyInProgress"
      ) {
        // Ignore since it means we can continue with an existing registration
        return;
      }
      throw error;
    }
  };

  #registerWithOpenId = async (
    jwt: string,
    name: string,
    configIssuer: string,
    discoveryDomain?: string,
  ): Promise<bigint> => {
    const result = await this.#openIdRegistrationCommit(
      jwt,
      name,
      discoveryDomain,
    );
    if (this.#options.trackLastUsed) {
      if (discoveryDomain !== undefined) {
        // A discovery domain means this JWT was redeemed through on-demand
        // SSO discovery, not a static `openid_configs` provider (this is the
        // 1-click SSO sign-up path, which reuses `continueWithOpenId`). Track
        // it as `sso` — keyed by domain — so re-auth re-runs discovery;
        // recording it as `openid` would break sign-in, whose SSO branch
        // needs the domain. Mirrors `#registerWithSso`.
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber: result.identityNumber,
          name,
          authMethod: {
            sso: {
              domain: discoveryDomain,
              email: result.decodedJwt.email,
              loginHint: result.loginHint,
            },
          },
          createdAtMillis: Date.now(),
        });
      } else {
        const { name: jwtName, email, ...restJWTClaims } = result.decodedJwt;
        const metadata: MetadataMapV2 = [];
        if (jwtName !== undefined) {
          metadata.push(["name", { String: jwtName }]);
        }
        if (email !== undefined) {
          metadata.push(["email", { String: email }]);
        }
        extractIssuerTemplateClaims(configIssuer).forEach((key) => {
          if (restJWTClaims[key] !== undefined) {
            metadata.push([key, { String: restJWTClaims[key] }]);
          }
        });
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber: result.identityNumber,
          name,
          authMethod: {
            openid: {
              iss: result.iss,
              sub: result.sub,
              loginHint: result.loginHint,
              metadata,
            },
          },
          createdAtMillis: Date.now(),
        });
      }
    }
    return result.identityNumber;
  };

  #registerWithSso = async (
    jwt: string,
    name: string,
    domain: string,
    ssoName: string | undefined,
  ): Promise<bigint> => {
    const result = await this.#openIdRegistrationCommit(jwt, name, domain);
    if (this.#options.trackLastUsed) {
      // See `continueWithSso`: email is kept only for the identity-row
      // display fallback chain (email → name → domain).
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber: result.identityNumber,
        name,
        authMethod: {
          sso: {
            domain,
            name: ssoName,
            email: result.decodedJwt.email,
            loginHint: result.loginHint,
          },
        },
        createdAtMillis: Date.now(),
      });
    }
    return result.identityNumber;
  };

  // Shared core for `#registerWithOpenId` / `#registerWithSso`. Calls the
  // canister-side `openid_identity_registration_finish` (with captcha-retry
  // recursion), authenticates with the resulting delegation, and returns
  // the credential identifiers + decoded JWT — leaving LastUsedIdentity
  // recording to the variant-specific callers.
  #openIdRegistrationCommit = async (
    jwt: string,
    name: string,
    discoveryDomain?: string,
  ): Promise<{
    iss: string;
    sub: string;
    loginHint?: string;
    identityNumber: bigint;
    decodedJwt: ReturnType<typeof decodeJWT>;
  }> => {
    try {
      // An SSO discovery / JWKS cache that's cold (or has since been evicted)
      // reports `Pending`; retry until it warms instead of failing the signup.
      try {
        await retryWhilePending(() =>
          get(sessionStore).actor.openid_identity_registration_finish({
            jwt,
            salt: get(sessionStore).salt,
            name,
            discovery_domain:
              discoveryDomain !== undefined ? [discoveryDomain] : [],
            origin: this.#sso !== undefined ? [this.#sso.origin] : [],
          }),
        ).then(throwCanisterError);
      } catch (error) {
        if (
          isCanisterError<IdRegFinishError>(error) &&
          error.type === "SsoNormalLoginRequired"
        ) {
          throw new SsoNormalLoginRequiredError();
        }
        throw error;
      }
      const decodedJwt = decodeJWT(jwt);
      const { iss, sub, loginHint } = decodedJwt;
      const { identity, identityNumber } =
        this.#sso !== undefined && discoveryDomain !== undefined
          ? await authenticateWithSso({
              canisterId,
              session: get(sessionStore),
              jwt,
              discoveryDomain,
              origin: this.#sso.origin,
            })
          : await authenticateWithJWT({
              canisterId,
              session: get(sessionStore),
              jwt,
              discoveryDomain,
            });
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulOpenIDRegistration,
      );
      await authenticationStore.set({
        identity,
        identityNumber,
        authMethod: { openid: { iss, sub } },
      });
      this.#captcha = undefined;
      return { iss, sub, loginHint, identityNumber, decodedJwt };
    } catch (error) {
      if (
        isCanisterError<IdRegFinishError>(error) &&
        error.type === "UnexpectedCall"
      ) {
        const nextStep = error.value(error.type).next_step;
        if ("CheckCaptcha" in nextStep) {
          await this.#solveCaptcha(
            `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`,
          );
          return this.#openIdRegistrationCommit(jwt, name, discoveryDomain);
        }
      }
      throw error;
    }
  };
}
