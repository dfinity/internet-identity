<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import AccessLevelSelector from "$lib/components/ui/AccessLevelSelector.svelte";
  import {
    readAccessLevelPreference,
    writeAccessLevelPreference,
    type AccessLevel,
  } from "$lib/utils/accessLevel";
  import { READ_ONLY_MODE } from "$lib/state/featureFlags";
  import CliHeader from "../components/CliHeader.svelte";
  import TerminalBlock from "../components/TerminalBlock.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Hostname of the app the CLI is being authorized for, or undefined for
     *  generic mode. */
    domain?: string;
    onAuthorize: (accessLevel: AccessLevel) => Promise<void>;
  }

  const { domain, onAuthorize }: Props = $props();

  let busy = $state(false);
  // The access-level selector (shown only when READ_ONLY_MODE is on) starts on
  // the user's last CLI choice, or unselected on a first-time sign in so they
  // pick explicitly. While the flag is off (the current default), the selector
  // is hidden and `effectiveAccessLevel` forces full access, so this state is
  // never surfaced (a queries-only delegation would fail closed in every
  // current agent — see the READ_ONLY_MODE flag).
  let accessLevel: AccessLevel | undefined = $state(
    readAccessLevelPreference("cli"),
  );
  // With the flag on, the selector gates "Continue" until a choice is made, so
  // `accessLevel` is always defined here; the fallback only satisfies the type.
  const effectiveAccessLevel: AccessLevel = $derived(
    $READ_ONLY_MODE ? (accessLevel ?? "full-access") : "full-access",
  );
  // First-time sign in must choose before continuing (flag on only).
  const mustChooseAccess = $derived(
    $READ_ONLY_MODE && accessLevel === undefined,
  );

  const handleClick = async () => {
    busy = true;
    try {
      if ($READ_ONLY_MODE && accessLevel !== undefined) {
        writeAccessLevelPreference("cli", accessLevel);
      }
      await onAuthorize(effectiveAccessLevel);
    } finally {
      busy = false;
    }
  };

  // Title is short and constant; the app hostname lives in the badge under
  // the connector visual (CliHeader handles it).
  const isAppMode = $derived(domain !== undefined);
  const command = $derived(
    isAppMode
      ? `icp identity link web --app ${domain}`
      : "icp identity link web",
  );
</script>

<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <CliHeader appOrigin={isAppMode ? `https://${domain}` : undefined} />

    <h1 class="text-text-primary mt-2 text-2xl font-medium">
      {isAppMode ? $t`Allow CLI access` : $t`Sign in`}
    </h1>
    <p class="text-text-tertiary mt-1 text-base">
      {isAppMode
        ? $t`with your identity in this app`
        : $t`with your Internet Identity`}
    </p>

    <div class="mt-6">
      <TerminalBlock
        {command}
        progressLine={$t`Linking Internet Identity`}
        {busy}
      />
    </div>

    <p class="text-text-secondary mt-4 text-[13px] leading-normal text-pretty">
      {#if isAppMode}
        <Trans>The CLI uses your account in this app.</Trans>
      {:else}
        <Trans>
          The CLI gets its own account, separate from your other apps.
        </Trans>
      {/if}
    </p>

    {#if $READ_ONLY_MODE}
      <AccessLevelSelector bind:accessLevel disabled={busy} class="mt-4" />
    {/if}

    <button
      class="btn btn-primary btn-xl mt-6 w-full"
      onclick={handleClick}
      disabled={busy || mustChooseAccess}
    >
      {#if busy}
        <ProgressRing class="size-5" />
        <span>{isAppMode ? $t`Allowing access` : $t`Signing in`}</span>
      {:else}
        <span>{isAppMode ? $t`Allow access` : $t`Continue`}</span>
      {/if}
    </button>
  </AuthPanel>
</div>
