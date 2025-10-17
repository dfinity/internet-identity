import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";
import {
  authenticateWithJWT,
  authenticateWithPasskey,
  authenticateWithSession,
} from "$lib/utils/authentication";
import { canisterConfig, canisterId } from "$lib/globals";
import {
  authenticatedStore,
  authenticationStore,
} from "$lib/stores/authentication.store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { sessionStore } from "$lib/stores/session.store";
import { get } from "svelte/store";
import { features } from "$lib/legacy/features";
import { isNullish, nonNullish } from "@dfinity/utils";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import {
  CheckCaptchaError,
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
} from "$lib/utils/openID";
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
  >("chooseMethod");
  #captcha = $state<{
    image: string;
    attempt: number;
    solve: (solution: string) => void;
  }>();
  #systemOverlay = $state(false);
  #confirmationCode = $state<string>();
  #name = $state<string>();
  #jwt = $state<string>();
  #configIssuer = $state<string>();

  get view() {
    return this.#view;
  }

  get captcha() {
    return this.#captcha;
  }

  get systemOverlay() {
    return this.#systemOverlay;
  }

  get confirmationCode() {
    return this.#confirmationCode;
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

  continueWithExistingPasskey = async (): Promise<bigint> => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.UseExistingPasskey);
    const { identity, identityNumber, credentialId } =
      await authenticateWithPasskey({
        canisterId,
        session: get(sessionStore),
      });
    await authenticationStore.set({ identity, identityNumber });
    const info =
      await get(authenticatedStore).actor.get_anchor_info(identityNumber);
    if (this.#options.trackLastUsed) {
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: { passkey: { credentialId } },
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
    if (isNullish(this.#name)) {
      throw new Error("Name is not set");
    }
    const passkeyIdentity =
      features.DUMMY_AUTH || nonNullish(canisterConfig.dummy_auth[0]?.[0])
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
    let jwt: string | undefined = existingJwt;
    // Convert OpenIdConfig to RequestConfig
    const requestConfig: RequestConfig = {
      clientId: config.client_id,
      authURL: config.auth_uri,
      authScope: config.auth_scope.join(" "),
      configURL: config.fedcm_uri?.[0],
    };
    authenticationV2Funnel.addProperties({
      provider: config.name,
    });
    if (isNullish(jwt)) {
      // Create two try-catch blocks to avoid double-triggering the analytics.
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
        // Moved after `requestJWT` to avoid Safari from blocking the popup.
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
      // If the previous call succeeds, it means the OpenID user already exists in II.
      // Therefore, they are logging in.
      // If the call fails, it means the OpenID user does not exist in II.
      // In that case, we register them.
      authenticationV2Funnel.trigger(AuthenticationV2Events.LoginWithOpenID);
      await authenticationStore.set({ identity, identityNumber });
      const info =
        await get(authenticatedStore).actor.get_anchor_info(identityNumber);
      const authnMethod = info.openid_credentials[0]?.find(
        (method) => method.iss === iss,
      );
      if (this.#options.trackLastUsed) {
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber,
          name: info.name[0],
          authMethod: {
            openid: { iss, sub, metadata: authnMethod?.metadata, loginHint },
          },
          createdAtMillis: info.created_at.map(nanosToMillis)[0],
        });
      }
      return { identityNumber, type: "signIn" };
    } catch (error) {
      if (
        isCanisterError<OpenIdDelegationError>(error) &&
        error.type === "NoSuchAnchor" &&
        nonNullish(jwt)
      ) {
        this.#jwt = jwt;
        this.#configIssuer = config.issuer;
        const { name } = decodeJWT(jwt);
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.RegisterWithOpenID,
        );
        if (isNullish(name)) {
          // Show enter name screen to complete registration,
          this.#view = "setupNewIdentity";
        }
        return { name, type: "signUp" };
      }
      throw error;
    }
  };

  completeOpenIdRegistration = async (name: string): Promise<bigint> => {
    if (isNullish(this.#jwt) || isNullish(this.#configIssuer)) {
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
    const uaParser = loadUAParser();
    const alias = await inferPasskeyAlias({
      authenticatorType: passkeyIdentity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: passkeyIdentity.getAaguid(),
    });
    const authnMethod = passkeyAuthnMethodData({
      alias,
      pubKey: passkeyIdentity.getPublicKey().toDer(),
      credentialId: passkeyIdentity.getCredentialId()!,
      authenticatorAttachment: passkeyIdentity.getAuthenticatorAttachment(),
      origin: window.location.origin,
    });
    const name = passkeyIdentity.getName();
    try {
      const { identity_number: identityNumber } = await get(sessionStore)
        .actor.identity_registration_finish({
          name: nonNullish(name) ? [name] : [],
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
      await authenticationStore.set({ identity, identityNumber });
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
    try {
      await get(sessionStore)
        .actor.openid_identity_registration_finish({
          jwt,
          salt: get(sessionStore).salt,
          name,
        })
        .then(throwCanisterError);
      const {
        iss,
        sub,
        loginHint,
        name: jwtName,
        email,
        ...restJWTClaims
      } = decodeJWT(jwt);
      const { identity, identityNumber } = await authenticateWithJWT({
        canisterId,
        session: get(sessionStore),
        jwt,
      });
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulOpenIDRegistration,
      );
      await authenticationStore.set({ identity, identityNumber });
      const metadata: MetadataMapV2 = [];
      if (nonNullish(jwtName)) {
        metadata.push(["name", { String: jwtName }]);
      }
      if (nonNullish(email)) {
        metadata.push(["email", { String: email }]);
      }
      const claimKeys = extractIssuerTemplateClaims(configIssuer);
      if (nonNullish(claimKeys)) {
        claimKeys.forEach((key) => {
          if (nonNullish(restJWTClaims[key])) {
            metadata.push([
              key,
              {
                String: restJWTClaims[key],
              },
            ]);
          }
        });
      }
      if (this.#options.trackLastUsed) {
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber,
          name,
          authMethod: { openid: { iss, sub, loginHint, metadata } },
          createdAtMillis: Date.now(),
        });
      }
      this.#captcha = undefined;
      return identityNumber;
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
          return this.#registerWithOpenId(jwt, name, configIssuer);
        }
      }
      throw error;
    }
  };
}
