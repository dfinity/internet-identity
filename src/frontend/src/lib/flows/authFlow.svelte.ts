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
  createGoogleRequestConfig,
  requestJWT,
  RequestConfig,
  decodeJWT,
  GOOGLE_ISSUER,
  extractIssuerTemplateClaims,
} from "$lib/utils/openID";

export class AuthFlow {
  #view = $state<
    | "chooseMethod"
    | "setupOrUseExistingPasskey"
    | "setupNewPasskey"
    | "infoPasskey"
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
  abTestGroup: "infoPasskey" | "default";

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

  constructor() {
    this.chooseMethod();
    const isE2E = nonNullish(canisterConfig.dummy_auth[0]?.[0]);
    // No A/B test in E2E runs
    const GROUP_INFO_PERCENTAGE = isE2E ? -1 : 0.2;
    this.abTestGroup =
      Math.random() < GROUP_INFO_PERCENTAGE ? "infoPasskey" : "default";
  }

  chooseMethod = (): void => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.SelectMethodScreen, {
      abTestGroup: this.abTestGroup,
    });
    this.#view = "chooseMethod";
  };

  setupOrUseExistingPasskey = (): void => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.ContinueWithPasskeyScreen,
      { abTestGroup: this.abTestGroup },
    );
    this.#view = "setupOrUseExistingPasskey";
  };

  continueWithExistingPasskey = async (): Promise<bigint> => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.UseExistingPasskey, {
      abTestGroup: this.abTestGroup,
    });
    const { identity, identityNumber, credentialId } =
      await authenticateWithPasskey({
        canisterId,
        session: get(sessionStore),
      });
    await authenticationStore.set({ identity, identityNumber });
    const info =
      await get(authenticatedStore).actor.get_anchor_info(identityNumber);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber,
      name: info.name[0],
      authMethod: { passkey: { credentialId } },
    });
    return identityNumber;
  };

  setupNewPasskey = (): void => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.EnterNameScreen, {
      abTestGroup: this.abTestGroup,
    });
    this.#view = "setupNewPasskey";
  };

  submitNameAndContinue = async (
    name: string,
  ): Promise<undefined | { type: "created"; identityNumber: bigint }> => {
    this.#name = name;
    if (this.abTestGroup === "infoPasskey") {
      authenticationV2Funnel.trigger(AuthenticationV2Events.InfoPasskeyScreen, {
        abTestGroup: this.abTestGroup,
      });
      this.#view = "infoPasskey";
      return;
    } else {
      return { type: "created", identityNumber: await this.createPasskey() };
    }
  };

  createPasskey = async (): Promise<bigint> => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.StartWebauthnCreation,
      { abTestGroup: this.abTestGroup },
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

  continueWithGoogle = async (): Promise<{
    identityNumber: bigint;
    type: "signIn" | "signUp";
  }> => {
    let jwt: string | undefined = undefined;
    const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id;
    if (isNullish(clientId)) {
      throw new Error("Google is not configured");
    }
    authenticationV2Funnel.addProperties({
      provider: "Google",
    });
    // Create two try-catch blocks to avoid double-triggering the analytics.
    try {
      const requestConfig = createGoogleRequestConfig(clientId);
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
      authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueWithOpenID);
    }
    try {
      const { iss, sub, loginHint } = decodeJWT(jwt);
      const { identity, identityNumber } = await authenticateWithJWT({
        canisterId,
        session: get(sessionStore),
        jwt,
      });
      // If the previous call succeeds, it means the Google user already exists in II.
      // Therefore, they are logging in.
      // If the call fails, it means the Google user does not exist in II.
      // In that case, we register them.
      authenticationV2Funnel.trigger(AuthenticationV2Events.LoginWithOpenID);
      await authenticationStore.set({ identity, identityNumber });
      const info =
        await get(authenticatedStore).actor.get_anchor_info(identityNumber);
      const authnMethod = info.openid_credentials[0]?.find(
        (method) => method.iss === iss,
      );
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: {
          openid: { iss, sub, loginHint, metadata: authnMethod?.metadata },
        },
      });
      return { identityNumber, type: "signIn" };
    } catch (error) {
      if (
        isCanisterError<OpenIdDelegationError>(error) &&
        error.type === "NoSuchAnchor" &&
        nonNullish(jwt)
      ) {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.RegisterWithOpenID,
        );
        await this.#startRegistration();
        const { name } = decodeJWT(jwt); // Google JWT always has a name
        const identityNumber = await this.#registerWithOpenId(
          jwt,
          name!,
          GOOGLE_ISSUER,
        );
        return { identityNumber, type: "signUp" };
      }
      throw error;
    }
  };

  continueWithOpenId = async (
    config: OpenIdConfig,
    useFullRedirect?: boolean,
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
    let jwt: string | undefined = undefined;
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
    // Create two try-catch blocks to avoid double-triggering the analytics.
    try {
      this.#systemOverlay = true;
      jwt = await requestJWT(requestConfig, {
        nonce: get(sessionStore).nonce,
        mediation: "required",
        useFullRedirect,
      });
    } catch (error) {
      this.#view = "chooseMethod";
      throw error;
    } finally {
      this.#systemOverlay = false;
      // Moved after `requestJWT` to avoid Safari from blocking the popup.
      authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueWithOpenID);
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
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: {
          openid: { iss, sub, metadata: authnMethod?.metadata, loginHint },
        },
      });
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
    authenticationV2Funnel.trigger(AuthenticationV2Events.RegisterWithPasskey, {
      abTestGroup: this.abTestGroup,
    });
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
        { abTestGroup: this.abTestGroup },
      );
      const credentialId = new Uint8Array(passkeyIdentity.getCredentialId()!);
      const identity = await authenticateWithSession({
        session: get(sessionStore),
      });
      await authenticationStore.set({ identity, identityNumber });
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: passkeyIdentity.getName(),
        authMethod: { passkey: { credentialId } },
      });
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
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name,
        authMethod: { openid: { iss, sub, loginHint, metadata } },
      });
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
