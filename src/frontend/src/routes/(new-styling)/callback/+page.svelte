<script lang="ts">
  import { analytics } from "$lib/utils/analytics/analytics";
  import { onMount } from "svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import {
    authorizationStatusStore,
    authorizationStore,
  } from "$lib/stores/authorization.store";
  import { throwCanisterError } from "$lib/utils/utils";
  import { handleError } from "$lib/components/utils/error";
  import AuthorizeError from "$lib/components/views/AuthorizeError.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { ArrowRightIcon } from "@lucide/svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { decodeJWT, findConfig, isOpenIdConfig } from "$lib/utils/openID";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import { t } from "$lib/stores/locale.store";

  analytics.event("page-redirect-callback");

  const status = $derived($authorizationStatusStore);
  let directAuthorizeOrigin = $state<string>();
  const dapps = getDapps();
  const dapp = $derived(
    dapps.find(
      (dapp) =>
        nonNullish(directAuthorizeOrigin) &&
        dapp.hasOrigin(directAuthorizeOrigin),
    ),
  );

  onMount(async () => {
    // If OpenID flow was opened within same window as II,
    // authorize with default account directly instead.
    const searchParams = new URLSearchParams(window.location.hash.slice(1));
    const redirectState = searchParams.get("state");
    const jwt = searchParams.get("id_token");
    const item = sessionStorage.getItem("ii-direct-authorize-openid");
    if (nonNullish(item)) {
      const { origin, state } = JSON.parse(item);
      if (
        nonNullish(redirectState) &&
        redirectState === state &&
        nonNullish(jwt)
      ) {
        directAuthorizeOrigin = origin;

        const authFlow = new AuthFlow({ trackLastUsed: false });
        const { iss, ...metadata } = decodeJWT(jwt);
        const config = findConfig(
          iss,
          Object.entries(metadata).map(([key, value]) => [
            key,
            { String: value! },
          ]),
        );
        if (isNullish(config) || !isOpenIdConfig(config)) {
          return;
        }
        const result = await authFlow.continueWithOpenId(config, jwt);
        if (result.type === "signUp") {
          await authFlow.completeOpenIdRegistration(result.name!);
        }
        authorizationStore.subscribe(async ({ context, status }) => {
          if (
            status !== "authenticating" ||
            isNullish(context) ||
            isNullish($authenticationStore)
          ) {
            return;
          }
          try {
            const account = await $authenticationStore.actor
              .get_default_account(
                $authenticationStore.identityNumber,
                context.effectiveOrigin,
              )
              .then(throwCanisterError);
            await authorizationStore.authorize(account.account_number[0]);
          } catch (error) {
            handleError(error);
          }
        });
        await authorizationStore.init({ allowedOrigin: origin });
        return;
      }
    }

    // User was returned here after redirect from a OpenID flow callback,
    // these flows are always handled in a popup and the callback url is
    // returned to the opener window through the PostMessage API.
    window.opener.postMessage(window.location.href, window.location.origin);
  });
</script>

{#if nonNullish(directAuthorizeOrigin)}
  <div class="flex min-h-[100dvh] flex-col items-center justify-center px-8">
    {#if nonNullish(dapp?.logoSrc)}
      {@const name = dapp?.name ?? directAuthorizeOrigin}
      <img
        src={dapp?.logoSrc}
        alt={$t`${name} logo`}
        class="mb-10 h-16 max-w-50 object-contain"
      />
    {/if}
    <p class="text-text-secondary mb-1 text-xl font-semibold">
      {$t`Signing in securely`}
    </p>
    <p class="text-text-tertiary text-sm">{$t`This takes a few seconds.`}</p>
    <div class="bg-bg-quaternary my-6 h-0.5 w-full max-w-74 rounded-full">
      <div class="bg-fg-brand-primary animate-grow h-full rounded-full"></div>
    </div>
    <p class="text-text-secondary text-base">
      {$t`Powered by Internet Identity`}
    </p>
    <Button
      href={window.location.origin}
      target="_blank"
      variant="tertiary"
      class="mt-10"
      size="sm"
    >
      <span>{$t`How it works`}</span>
      <ArrowRightIcon class="size-4" />
    </Button>
  </div>
{/if}

<!-- Renders any error status or late success status dialog when needed -->
<AuthorizeError {status} />

<style>
  @keyframes grow {
    from {
      width: 0;
    }
    to {
      width: 100%;
    }
  }

  .animate-grow {
    animation: grow 6s ease-out forwards;
  }
</style>
