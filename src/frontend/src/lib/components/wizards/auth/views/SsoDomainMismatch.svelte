<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import {
    ChevronDownIcon,
    CircleAlertIcon,
    LogInIcon,
    UserIcon,
  } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    /**
     * The discovery domain the user just signed in through; undefined for a
     * configured provider (Google / Microsoft / Apple), where no domain was
     * entered.
     */
    enteredDomain?: string;
    /**
     * The discovery domain the credential is registered through, as reported
     * by the canister (`SsoDomainMismatch.registered_sso_domain`). Undefined
     * when the stored credential carries no domain stamp.
     */
    registeredDomain?: string;
    providerName?: string;
    userName?: string;
    userEmail?: string;
    /**
     * Whether the SSO discovery for `registeredDomain` (needed to sign in
     * through it) is still resolving, ready, or failed. Ignored when there is
     * no `registeredDomain`.
     */
    retryState: "loading" | "ready" | "failed";
    onSignInWithRegisteredDomain: () => void;
    onRecover: () => void;
    onCancel?: () => void;
    loading?: boolean;
  }

  let {
    enteredDomain,
    registeredDomain,
    providerName: providerNameProp,
    userName,
    userEmail,
    retryState,
    onSignInWithRegisteredDomain,
    onRecover,
    onCancel,
    loading = false,
  }: Props = $props();

  const providerName = $derived(
    providerNameProp ?? enteredDomain ?? registeredDomain ?? "",
  );
  const resolvedUserName = $derived(userName ?? userEmail ?? providerName);
  const resolvedUserEmail = $derived(
    userName !== undefined ? userEmail : undefined,
  );
</script>

<div class="flex flex-col">
  <div class="flex flex-col items-start">
    <h2
      class="text-text-primary text-[22px] leading-[26.4px] font-medium tracking-tight"
    >
      {$t`Linked through another SSO domain`}
    </h2>
    <p class="text-text-tertiary mt-2 max-w-80 text-sm leading-5">
      {#if registeredDomain !== undefined && enteredDomain !== undefined}
        {$t`This ${providerName} account is already linked to an Internet Identity, but through ${registeredDomain} rather than ${enteredDomain}.`}
      {:else if registeredDomain !== undefined}
        {$t`This ${providerName} account is already linked to an Internet Identity through the SSO domain ${registeredDomain}.`}
      {:else if enteredDomain !== undefined}
        {$t`This ${providerName} account is already linked to an Internet Identity, but through a different SSO domain than ${enteredDomain}.`}
      {:else}
        {$t`This ${providerName} account is already linked to an Internet Identity through an SSO domain.`}
      {/if}
    </p>
  </div>

  <div class="mt-4 flex flex-col items-center py-5">
    <span
      class="bg-bg-primary border-border-secondary text-fg-disabled flex size-16 items-center justify-center rounded-full border"
    >
      <UserIcon class="size-7" aria-hidden="true" />
    </span>
    <div class="mt-2 text-center">
      <div class="text-text-primary text-base leading-tight font-semibold">
        {resolvedUserName}
      </div>
      {#if resolvedUserEmail !== undefined}
        <div class="text-text-tertiary mt-0.5 text-[13px]">
          {resolvedUserEmail}
        </div>
      {/if}
    </div>
    <span
      class="bg-bg-primary border-border-secondary text-text-tertiary mt-2 inline-flex items-center gap-1.5 rounded-full border px-2.5 py-1 text-xs font-semibold"
    >
      <CircleAlertIcon
        class="text-fg-warning-primary size-3.5"
        aria-hidden="true"
      />
      {#if registeredDomain !== undefined}
        {$t`Linked through ${registeredDomain}`}
      {:else}
        {$t`Linked through another domain`}
      {/if}
    </span>
  </div>

  {#if registeredDomain !== undefined}
    <button
      onclick={onSignInWithRegisteredDomain}
      disabled={loading || retryState !== "ready"}
      class="btn btn-primary btn-lg mt-3 w-full gap-2"
    >
      {#if loading || retryState === "loading"}
        <ProgressRing class="size-4" />
      {:else}
        <LogInIcon class="size-4" aria-hidden="true" />
      {/if}
      {$t`Sign in with ${registeredDomain}`}
    </button>
    {#if retryState === "failed"}
      <p class="text-text-error-primary mt-2 text-center text-sm" role="alert">
        {$t`Couldn't load the SSO settings of ${registeredDomain}. Try again later or use a different method.`}
      </p>
    {/if}
  {/if}

  {#if onCancel !== undefined}
    <button
      onclick={onCancel}
      disabled={loading}
      class="text-text-tertiary hover:text-text-primary mt-3 self-center text-sm font-semibold outline-0 hover:underline focus-visible:underline"
    >
      {$t`Use a different method`}
    </button>
  {/if}

  <details
    class="group bg-bg-secondary dark:bg-bg-primary border-border-secondary -mx-4 mt-4 -mb-4 border-t px-4 py-4 sm:-mx-6 sm:-mb-8 sm:px-6"
  >
    <summary
      class="text-text-tertiary focus-visible:ring-focus-ring focus-visible:ring-offset-bg-primary flex cursor-pointer list-none items-center justify-between rounded-sm text-sm font-semibold outline-none focus-visible:ring-2 focus-visible:ring-offset-2"
    >
      <span>{$t`Why am I seeing this?`}</span>
      <ChevronDownIcon
        class="size-4 transition-transform group-open:rotate-180"
        aria-hidden="true"
      />
    </summary>
    <div class="text-text-tertiary mt-3 text-sm leading-5">
      <p>
        {$t`An account linked through SSO signs in through the domain it was linked with. This account is linked to an identity, so it can't be used to create a new one.`}
        {#if enteredDomain !== undefined}
          {$t`${enteredDomain} isn't the domain it was linked through.`}
        {:else}
          {$t`Signing in with the provider directly doesn't reach it.`}
        {/if}
      </p>
      <div
        class="border-border-secondary mt-3 flex items-center justify-between border-t pt-2.5"
      >
        <span>{$t`Lost access to your identity?`}</span>
        <button
          onclick={onRecover}
          class="text-text-primary font-semibold outline-0 hover:underline focus-visible:underline"
        >
          {$t`Recover`}
        </button>
      </div>
    </div>
  </details>
</div>
