<script lang="ts">
  import { onMount } from "svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import {
    validateDomain,
    isDomainAllowed,
    discoverSsoConfig,
  } from "$lib/utils/ssoDiscovery";
  import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    continueWithSso: (result: SsoDiscoveryResult) => Promise<void | "cancelled">;
    goBack: () => void;
  }

  const { continueWithSso, goBack }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let domain = $state("");
  let error = $state<string>();
  let isDiscovering = $state(false);
  let discoveryResult = $state<SsoDiscoveryResult>();
  let debounceTimer = $state<ReturnType<typeof setTimeout>>();
  let isSubmitting = $state(false);

  const DEBOUNCE_MS = 750;

  /** Validate and run two-hop discovery in background on keystroke. */
  const handleInput = () => {
    // Clear previous state
    error = undefined;
    discoveryResult = undefined;

    if (debounceTimer !== undefined) {
      clearTimeout(debounceTimer);
    }

    const trimmed = domain.trim().toLowerCase();
    if (trimmed.length === 0) {
      return;
    }

    // Immediate domain format validation
    try {
      validateDomain(trimmed);
    } catch (e) {
      if (trimmed.includes(".")) {
        error = e instanceof Error ? e.message : "Invalid domain";
      }
      return;
    }

    // Check allowlist immediately
    if (!isDomainAllowed(trimmed)) {
      error = "This organization is not configured for SSO sign-in.";
      return;
    }

    // Debounced discovery fetch
    debounceTimer = setTimeout(async () => {
      isDiscovering = true;
      error = undefined;
      try {
        discoveryResult = await discoverSsoConfig(trimmed);
      } catch (e) {
        error =
          e instanceof Error
            ? e.message
            : "SSO discovery failed. Please try again.";
        discoveryResult = undefined;
      } finally {
        isDiscovering = false;
      }
    }, DEBOUNCE_MS);
  };

  const handleSubmit = async () => {
    if (discoveryResult === undefined) {
      return;
    }
    isSubmitting = true;
    try {
      await continueWithSso(discoveryResult);
    } finally {
      isSubmitting = false;
    }
  };

  onMount(() => {
    inputRef?.focus();
    return () => {
      if (debounceTimer !== undefined) {
        clearTimeout(debounceTimer);
      }
    };
  });
</script>

<div class="flex flex-1 flex-col">
  <div
    class="text-text-primary mb-8 flex w-full flex-col items-center justify-center"
  >
    <div class="my-5 flex items-center justify-center">
      <SsoIcon class="text-text-tertiary size-12" />
    </div>
    <div>
      <h1 class="mb-3 text-2xl font-medium sm:text-center">
        {$t`Sign in with SSO`}
      </h1>
      <p
        class="text-text-tertiary text-base font-medium text-balance sm:text-center"
      >
        {$t`Enter your organization's domain to sign in with your corporate identity provider.`}
      </p>
    </div>
  </div>
  <form
    class="flex flex-col items-stretch gap-6"
    onsubmit={(e) => {
      e.preventDefault();
      handleSubmit();
    }}
  >
    <Input
      bind:element={inputRef}
      bind:value={domain}
      oninput={handleInput}
      inputmode="url"
      placeholder={$t`e.g. dfinity.org`}
      type="text"
      size="md"
      autocomplete="off"
      autocorrect="off"
      autocapitalize="off"
      spellcheck="false"
      disabled={isSubmitting}
      {error}
      aria-label={$t`Organization domain`}
    >
      {#snippet hint()}
        {#if isDiscovering}
          <span class="flex items-center gap-1.5">
            <ProgressRing class="size-3.5" />
            {$t`Looking up SSO configuration...`}
          </span>
        {:else if discoveryResult !== undefined}
          <span class="text-text-success"
            >{$t`SSO configuration found. Click Continue to sign in.`}</span
          >
        {/if}
      {/snippet}
    </Input>
    <Button
      onclick={handleSubmit}
      variant="primary"
      size="lg"
      type="submit"
      disabled={discoveryResult === undefined || isSubmitting || isDiscovering}
    >
      {#if isSubmitting}
        <ProgressRing />
        <span>{$t`Signing in...`}</span>
      {:else}
        <span>{$t`Continue`}</span>
      {/if}
    </Button>
    <button
      type="button"
      onclick={goBack}
      class="text-text-secondary text-sm font-semibold hover:underline focus-visible:underline outline-0 self-center"
    >
      {$t`Back to sign-in options`}
    </button>
  </form>
</div>
