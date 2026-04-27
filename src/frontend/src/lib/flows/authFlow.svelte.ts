import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";
import {
  authenticateWithJWT,
  authenticateWithPasskey,
  authenticateWithSession,
} from "$lib/utils/authentication";
import { frontendCanisterConfig, canisterId } from "$lib/globals";
import {
  authenticatedStore,
  authenticationStore,
} from "$lib/stores/authentication.store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { sessionStore } from "$lib/stores/session.store";
import { get } from "svelte/store";
import { features } from "$lib/legacy/features";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
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
  selectAuthScopes,
} from "$lib/utils/openID";
import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
import { nanosToMillis } from "$lib/utils/time";

interface AuthFlowOptions {
  trackLastUsed?: boolean;
}

export class AuthFlow {
  #options: Required<AuthFlowOptions>;
  #view = $state<
    | "chooseMethod"
    | "setupOrUseExistingPasskey"
    | "setupNewPasskey"
    | "setupNewIdentity"
    | "signInWithSso"
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
  // SSO sign-up state: kept separate from the direct-provider `#jwt` /
  // `#configIssuer` pair so `completeSsoRegistration` (the SSO-specific
  // counterpart to `completeOpenIdRegistration`) can reconstruct the
  // `sso` LastUsedIdentity entry without leaking SSO awareness into the
  // direct-provider methods.
  #ssoJwt = $state<string>();
  #ssoDomain = $state<string>();
  #ssoName = $state<string>();

  get view() {
    return this.#view;
  }

  get captcha() {
    return this.#captcha;
  }

  get systemOverlay() {
    return this.#systemOverlay;
  }

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
  ): Promise<
    | {
        identityNumber: bigint;
        type: "signIn";
      }
    | {
        name?: string;
        type: "signUp";
      }
  > => {
    const { clientId, discovery, domain, name: ssoName } = ssoResult;
    authenticationV2Funnel.addProperties({ provider: "SSO" });
    const result = await this.#openIdJwtSignIn({
      clientId,
      authURL: discovery.authorization_endpoint,
      authScope: selectAuthScopes(discovery.scopes_supported).join(" "),
    });
    if (result.type === "signIn") {
      if (this.#options.trackLastUsed) {
        // `email` is purely for display fallback in the identity row
        // (email → name → domain); decoded fresh from the JWT here so
        // the sso variant doesn't need to remember `iss`/`sub`.
        const { email } = decodeJWT(result.jwt);
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber: result.identityNumber,
          name: result.info.name[0],
          authMethod: {
            sso: {
              domain,
              name: ssoName,
              email,
              loginHint: result.loginHint,
            },
          },
          createdAtMillis: result.info.created_at.map(nanosToMillis)[0],
        });
      }
      return { identityNumber: result.identityNumber, type: "signIn" };
    }
    this.#ssoJwt = result.jwt;
    this.#ssoDomain = domain;
    this.#ssoName = ssoName;
    return { name: result.suggestedName, type: "signUp" };
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

  continueWithExistingPasskey = async (): Promise<bigint> => {
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
    if (this.#options.trackLastUsed) {
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod,
        createdAtMillis: info.created_at.map(nanosToMillis)[0],
      });
    }
    return identityNumber;
  };

  setupNewPasskey = (): void => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.EnterNameScreen);
    this.#view = "setupNewPasskey";
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
  ): Promise<
    | {
        identityNumber: bigint;
        type: "signIn";
      }
    | {
        name?: string;
        type: "signUp";
      }
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
    );
    if (result.type === "signIn") {
      if (this.#options.trackLastUsed) {
        const authnMethod = result.info.openid_credentials[0]?.find(
          (method) => method.iss === result.iss,
        );
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber: result.identityNumber,
          name: result.info.name[0],
          authMethod: {
            openid: {
              iss: result.iss,
              sub: result.sub,
              loginHint: result.loginHint,
              metadata: authnMethod?.metadata,
            },
          },
          createdAtMillis: result.info.created_at.map(nanosToMillis)[0],
        });
      }
      return { identityNumber: result.identityNumber, type: "signIn" };
    }
    this.#jwt = result.jwt;
    this.#configIssuer = config.issuer;
    return { name: result.suggestedName, type: "signUp" };
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
      }
  > => {
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
      const { identity, identityNumber } = await authenticateWithJWT({
        canisterId,
        session: get(sessionStore),
        jwt,
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
        const { name } = decodeJWT(jwt);
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.RegisterWithOpenID,
        );
        if (name === undefined) {
          this.#view = "setupNewIdentity";
        }
        return { type: "signUp", jwt, suggestedName: name };
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
    return this.#registerWithOpenId(this.#jwt, name, this.#configIssuer);
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
  ): Promise<bigint> => {
    const result = await this.#openIdRegistrationCommit(jwt, name);
    if (this.#options.trackLastUsed) {
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
    return result.identityNumber;
  };

  #registerWithSso = async (
    jwt: string,
    name: string,
    domain: string,
    ssoName: string | undefined,
  ): Promise<bigint> => {
    const result = await this.#openIdRegistrationCommit(jwt, name);
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
  ): Promise<{
    iss: string;
    sub: string;
    loginHint?: string;
    identityNumber: bigint;
    decodedJwt: ReturnType<typeof decodeJWT>;
  }> => {
    try {
      await get(sessionStore)
        .actor.openid_identity_registration_finish({
          jwt,
          salt: get(sessionStore).salt,
          name,
        })
        .then(throwCanisterError);
      const decodedJwt = decodeJWT(jwt);
      const { iss, sub, loginHint } = decodedJwt;
      const { identity, identityNumber } = await authenticateWithJWT({
        canisterId,
        session: get(sessionStore),
        jwt,
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
          return this.#openIdRegistrationCommit(jwt, name);
        }
      }
      throw error;
    }
  };
}
