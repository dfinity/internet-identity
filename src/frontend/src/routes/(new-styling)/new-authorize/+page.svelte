<script lang="ts">
  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import CenterCard from "$lib/components/UI/CenterCard.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { Connection, creationOptions } from "$lib/utils/iiConnection";
  import { readCanisterId, readCanisterConfig } from "$lib/utils/init";
  import type {
    AccountNumber,
    CheckCaptchaError,
    IdRegFinishError,
    IdRegStartError,
    OpenIdDelegationError,
  } from "$lib/generated/internet_identity_types";
  import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
  import {
    type AuthContext,
    authenticationProtocol,
  } from "$lib/flows/authorize/postMessageInterface";
  import { inferPasskeyAlias, loadUAParser } from "$lib/flows/register";
  import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
  import { fetchDelegation } from "$lib/flows/authorize/fetchDelegation";
  import type { PageProps } from "./$types";
  import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
  import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
  import { authenticateWithJWT } from "$lib/utils/authenticate/jwt";
  import { type State } from "./state";
  import ConnectOrCreatePasskey from "./components/ConnectOrCreatePasskey.svelte";
  import CreatePasskey from "./components/CreatePasskey.svelte";
  import SolveCaptcha from "./components/SolveCaptcha.svelte";
  import ContinueAs from "./components/ContinueAs.svelte";
  import Dialog from "$lib/components/UI/Dialog.svelte";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { handleError } from "./error";
  import { ProgressRing } from "@skeletonlabs/skeleton-svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import PickAuthenticationMethod from "./components/PickAuthenticationMethod.svelte";
  import { authenticateWithPasskey } from "$lib/utils/authenticate/passkey";
  import { Actor, HttpAgent, SignIdentity } from "@dfinity/agent";
  import PickAccount from "./components/PickAccount.svelte";

  const { data }: PageProps = $props();

  let currentState = $state<State>({ state: "loading", label: "Connecting" });
  let authContext = $state.raw<AuthContext>();
  let dappName = $derived<string>(
    authContext ? authContext?.requestOrigin : "",
  );

  let authenticate: (
    identity: SignIdentity,
    anchorNumber: bigint,
    accountNumber: AccountNumber | undefined,
    credentialId: ArrayBuffer | undefined,
    sub: string | undefined,
  ) => void;
  const connection = new Connection(readCanisterId(), readCanisterConfig());

  const pickAuthenticationMethod = () => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.SelectMethodScreen);
    currentState = { state: "pickAuthenticationMethod" };
  };

  const continueAs = (
    currentIdentity: LastUsedIdentity,
    identities: LastUsedIdentity[],
  ) => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsScreen);
    const nextState = {
      state: "continueAs",
      currentIdentity,
      identities,
      continueAs: async (identity) => {
        try {
          switch (identity.authMethod) {
            case "passkey": {
              authenticationV2Funnel.trigger(
                AuthenticationV2Events.ContinueAsPasskey,
              );
              const {
                identity: passkeyIdentity,
                anchorNumber,
                credentialId,
              } = await authenticateWithPasskey({
                credentialId: identity.credentialId,
                actor: data.session.actor,
              });
              return authenticate(
                passkeyIdentity,
                anchorNumber,
                undefined,
                credentialId,
                undefined,
              );
            }
            case "google": {
              authenticationV2Funnel.trigger(
                AuthenticationV2Events.ContinueAsGoogle,
              );
              const clientId =
                data.session.config.openid_google?.[0]?.[0]?.client_id;
              if (isNullish(clientId)) {
                return;
              }
              const requestConfig = createGoogleRequestConfig(clientId!);
              const jwt = await requestJWT(requestConfig, {
                nonce: data.session.nonce,
                mediation: "required",
                loginHint: identity.sub,
              });
              const {
                identity: jwtIdentity,
                anchorNumber,
                sub,
              } = await authenticateWithJWT({
                jwt,
                salt: data.session.salt,
                actor: data.session.actor,
              });
              return authenticate(
                jwtIdentity,
                anchorNumber,
                undefined,
                undefined,
                sub,
              );
            }
            default:
              void (identity.authMethod satisfies never);
          }
        } catch (error) {
          handleError(error);
          pickAuthenticationMethod();
        }
      },
      switchIdentity: (identity) => {
        currentState = { ...nextState, currentIdentity: identity };
      },
      useAnotherIdentity,
      useAnotherAccount,
    } satisfies State;
    currentState = nextState;
  };

  const useAnotherIdentity = () => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.UseAnother);
    pickAuthenticationMethod();
  };

  const useAnotherAccount = async (lastUsedIdentity: LastUsedIdentity) => {
    currentState = { state: "loading", label: "Authenticating" };
    switch (lastUsedIdentity.authMethod) {
      case "passkey": {
        const { identity, anchorNumber, credentialId } =
          await authenticateWithPasskey({
            credentialId: lastUsedIdentity.credentialId,
            actor: data.session.actor,
          });
        return pickAccount(identity, anchorNumber, credentialId, undefined);
      }
      case "google": {
        const clientId = data.session.config.openid_google?.[0]?.[0]?.client_id;
        if (isNullish(clientId)) {
          return;
        }
        const requestConfig = createGoogleRequestConfig(clientId!);
        const jwt = await requestJWT(requestConfig, {
          nonce: data.session.nonce,
          mediation: "required",
          loginHint: lastUsedIdentity.sub,
        });
        const { identity, anchorNumber, sub } = await authenticateWithJWT({
          jwt,
          salt: data.session.salt,
          actor: data.session.actor,
        });
        return pickAccount(identity, anchorNumber, undefined, sub);
      }
      default:
        void (lastUsedIdentity.authMethod satisfies never);
    }
  };

  const pickAccount = async (
    identity: SignIdentity,
    anchorNumber: bigint,
    credentialId: ArrayBuffer | undefined,
    sub: string | undefined,
  ) => {
    const agent = await HttpAgent.from(
      Actor.agentOf(data.session.actor) as HttpAgent,
    );
    agent.replaceIdentity(identity);
    const accounts = await data.session.actor.get_accounts.withOptions({
      agent,
    })(anchorNumber, authContext?.requestOrigin!);
    let currentAccountNumber = accounts[0].account_number[0];
    const nextState = {
      state: "pickAccount",
      accounts,
      currentAccountNumber: accounts[0].account_number[0],
      selectAccount: (accountNumber) => {
        currentAccountNumber = accountNumber;
        currentState = { ...nextState, currentAccountNumber };
      },
      createAccount: async (name) => {
        const account = await data.session.actor.create_account
          .withOptions({
            agent,
          })(anchorNumber, authContext?.requestOrigin!, name)
          .then(throwCanisterError);
        accounts.push(account);
        currentState = {
          ...nextState,
          accounts,
          currentAccountNumber: account.account_number[0],
        };
      },
      authenticate: () => {
        currentState = { state: "loading", label: "Authenticating" };
        authenticate(
          identity,
          anchorNumber,
          currentAccountNumber,
          credentialId,
          sub,
        );
      },
    } satisfies State;
    currentState = nextState;
  };

  const connectOrCreatePasskey = async () => {
    authenticationV2Funnel.trigger(
      AuthenticationV2Events.ContinueWithPasskeyScreen,
    );
    currentState = {
      state: "connectOrCreatePasskey",
      connect: continueWithExistingPasskey,
      create: createPasskey,
    };
  };

  const continueWithExistingPasskey = async () => {
    try {
      authenticationV2Funnel.trigger(AuthenticationV2Events.UseExistingPasskey);
      currentState = { state: "loading", label: "Authenticating" };
      const { identity, anchorNumber, credentialId } =
        await authenticateWithPasskey({ actor: data.session.actor });
      authenticate(identity, anchorNumber, undefined, credentialId, undefined);
    } catch (error) {
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const createPasskey = () => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.EnterNameScreen);
    currentState = {
      state: "createPasskey",
      create: async (name: string) => {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.StartWebauthnCreation,
        );
        currentState = { state: "loading", label: "Creating Passkey" };
        try {
          const passkeyIdentity = await DiscoverablePasskeyIdentity.create({
            publicKey: {
              ...creationOptions([], undefined, undefined),
              user: {
                id: window.crypto.getRandomValues(new Uint8Array(16)),
                name,
                displayName: name,
              },
            },
          });
          currentState = { state: "loading", label: "Creating Identity" };
          await startRegistration();
          await registerWithPasskey(passkeyIdentity);
        } catch (error) {
          handleError(error);
          pickAuthenticationMethod();
        }
      },
      cancel: connectOrCreatePasskey,
    };
  };

  const registerWithPasskey = async (
    passkeyIdentity: DiscoverablePasskeyIdentity,
    attempts = 0,
  ) => {
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
      const { identity_number } = await data.session.actor
        .identity_registration_finish({
          name: nonNullish(name) ? [name] : [],
          authn_method: authnMethod,
        })
        .then(throwCanisterError);
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulPasskeyRegistration,
      );
      currentState = { state: "loading", label: "Authenticating" };
      authenticate(
        passkeyIdentity,
        identity_number,
        undefined,
        passkeyIdentity.getCredentialId(),
        undefined,
      );
    } catch (error) {
      if (isCanisterError<IdRegFinishError>(error)) {
        switch (error.type) {
          case "UnexpectedCall":
            const nextStep = error.value(error.type).next_step;
            if ("CheckCaptcha" in nextStep) {
              if (attempts < 3) {
                await solveCaptcha(
                  `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`,
                );
                return registerWithPasskey(passkeyIdentity, attempts + 1);
              }
            }
            break;
          case "NoRegistrationFlow":
            if (attempts < 3) {
              // Apparently the flow has been cleaned up, try again.
              await startRegistration();
              return await registerWithPasskey(passkeyIdentity, attempts + 1);
            }
            break;
        }
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const continueWithGoogle = async () => {
    let jwt: string | undefined;
    try {
      authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueWithGoogle);
      currentState = { state: "loading", label: "Authenticating" };
      const clientId = data.session.config.openid_google?.[0]?.[0]?.client_id;
      if (isNullish(clientId)) {
        return;
      }
      const requestConfig = createGoogleRequestConfig(clientId!);
      jwt = await requestJWT(requestConfig, {
        nonce: data.session.nonce,
        mediation: "required",
      });
      const { identity, anchorNumber, sub } = await authenticateWithJWT({
        jwt,
        salt: data.session.salt,
        actor: data.session.actor,
      });
      // If the previous call succeeds, it means the Google user already exists in II.
      // Therefore, they are logging in.
      // If the call fails, it means the Google user does not exist in II.
      // In that case, we register them.
      authenticationV2Funnel.trigger(AuthenticationV2Events.LoginWithGoogle);
      authenticate(identity, anchorNumber, undefined, undefined, sub);
    } catch (error) {
      if (
        isCanisterError<OpenIdDelegationError>(error) &&
        error.type === "NoSuchAnchor" &&
        nonNullish(jwt)
      ) {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.RegisterWithGoogle,
        );
        currentState = { state: "loading", label: "Creating Identity" };
        await startRegistration();
        return registerWithGoogle(jwt);
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const startRegistration = async (): Promise<void> => {
    try {
      const { next_step } = await data.session.actor
        .identity_registration_start()
        .then(throwCanisterError);
      if ("CheckCaptcha" in next_step) {
        await solveCaptcha(
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
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const solveCaptcha = async (captcha: string, attempt = 0): Promise<void> =>
    new Promise((resolve) => {
      currentState = {
        state: "solveCaptcha",
        image: captcha,
        attempt,
        solve: async (solution) => {
          const nextCaptcha = await validateCaptcha(solution);
          if (nonNullish(nextCaptcha)) {
            await solveCaptcha(nextCaptcha, attempt + 1);
          }
          resolve();
        },
        cancel: pickAuthenticationMethod,
      };
    });

  const validateCaptcha = async (
    solution: string,
  ): Promise<string | undefined> => {
    try {
      await data.session.actor
        .check_captcha({ solution })
        .then(throwCanisterError);
    } catch (error) {
      if (
        isCanisterError<CheckCaptchaError>(error) &&
        error.type === "WrongSolution"
      ) {
        return `data:image/png;base64,${error.value(error.type).new_captcha_png_base64}`;
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  const registerWithGoogle = async (jwt: string) => {
    try {
      await data.session.actor
        .openid_identity_registration_finish({
          jwt,
          salt: data.session.salt,
        })
        .then(throwCanisterError);
      const { identity, anchorNumber, sub } = await authenticateWithJWT({
        jwt,
        salt: data.session.salt,
        actor: data.session.actor,
      });
      const result = await connection.fromDelegationIdentity(
        anchorNumber,
        identity,
      );
      authenticationV2Funnel.trigger(
        AuthenticationV2Events.SuccessfulGoogleRegistration,
      );
      authenticate(identity, anchorNumber, undefined, undefined, sub);
    } catch (error) {
      if (
        isCanisterError<IdRegFinishError>(error) &&
        error.type === "UnexpectedCall"
      ) {
        const nextStep = error.value(error.type).next_step;
        if ("CheckCaptcha" in nextStep) {
          await solveCaptcha(
            `data:image/png;base64,${nextStep.CheckCaptcha.captcha_png_base64}`,
          );
          return registerWithGoogle(jwt);
        }
      }
      handleError(error);
      pickAuthenticationMethod();
    }
  };

  authenticationProtocol({
    authenticate: (context) => {
      authContext = context;
      return new Promise((resolve) => {
        authenticate = async (
          identity,
          anchorNumber,
          _accountNumber, // TODO: Implement prepare/get account delegation
          credentialId,
          sub,
        ) => {
          const { connection: authenticatedConnection } =
            await connection.fromDelegationIdentity(anchorNumber, identity);
          const derivationOrigin =
            context.authRequest.derivationOrigin ?? context.requestOrigin;
          const [result, anchorInfo] = await Promise.all([
            fetchDelegation({
              connection: authenticatedConnection,
              derivationOrigin,
              publicKey: context.authRequest.sessionPublicKey,
              maxTimeToLive: context.authRequest.maxTimeToLive,
            }),
            authenticatedConnection.getAnchorInfo(),
          ]);
          if ("error" in result) {
            return;
          }
          const [userKey, parsed_signed_delegation] = result;
          lastUsedIdentitiesStore.addLatestUsed({
            identityNumber: authenticatedConnection.userNumber,
            name: anchorInfo.name[0],
            credentialId: nonNullish(credentialId)
              ? new Uint8Array(credentialId)
              : undefined,
            authMethod: nonNullish(credentialId) ? "passkey" : "google",
            sub,
          });
          resolve({
            kind: "success",
            delegations: [parsed_signed_delegation],
            userPublicKey: new Uint8Array(userKey),
            // This is a authnMethod forwarded to the app that requested authorization.
            // We don't want to leak which authnMethod was used.
            authnMethod: "passkey",
          });
        };

        authenticationV2Funnel.trigger(
          nonNullish(data.lastUsedIdentity)
            ? AuthenticationV2Events.LastUsedPresent
            : AuthenticationV2Events.LastUsedNotPresent,
        );
        if (nonNullish(data.lastUsedIdentity)) {
          continueAs(data.lastUsedIdentity, data.lastUsedIdentities);
        } else {
          pickAuthenticationMethod();
        }
      });
    },
    onProgress: () => {},
  });
</script>

<CenterContainer data-page="new-authorize-view">
  <CenterCard>
    {#if currentState.state === "loading"}
      <div class="flex flex-col items-center justify-center gap-2">
        <ProgressRing
          value={null}
          size="size-14"
          meterStroke="stroke-primary-900-100"
        />
        {#if nonNullish(currentState.label)}
          <p class="opacity-60">{currentState.label}</p>
        {/if}
      </div>
    {:else if currentState.state === "solveCaptcha"}
      <Dialog
        title={currentState.state === "solveCaptcha"
          ? "Prove you're not a robot"
          : "Continue with Passkey"}
        class="min-h-96 w-100"
      >
        <SolveCaptcha {...currentState} />
      </Dialog>
    {:else}
      <div class="mb-8 flex flex-col gap-1">
        <h1 class="h1">Sign in</h1>
        <p class="p font-medium">
          to continue with <span class="font-bold">{dappName.slice(7, 18)}</span
          >
        </p>
      </div>
      {#if currentState.state === "continueAs"}
        <ContinueAs {...currentState} />
      {:else if currentState.state === "pickAccount"}
        <PickAccount {...currentState} />
      {:else}
        <PickAuthenticationMethod
          {connectOrCreatePasskey}
          {continueWithGoogle}
        />
        {#if currentState.state === "connectOrCreatePasskey" || currentState.state === "createPasskey"}
          <Dialog
            title={"Continue with Passkey"}
            onClose={pickAuthenticationMethod}
            class="min-h-100 w-100"
          >
            {#if currentState.state === "connectOrCreatePasskey"}
              <ConnectOrCreatePasskey {...currentState} />
            {:else if currentState.state === "createPasskey"}
              <CreatePasskey {...currentState} />
            {/if}
          </Dialog>
        {/if}
      {/if}
    {/if}
  </CenterCard>
</CenterContainer>
