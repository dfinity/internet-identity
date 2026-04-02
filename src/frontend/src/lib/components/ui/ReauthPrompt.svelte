<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { UserCheckIcon } from "@lucide/svelte";
  import FeaturedIcon from "./FeaturedIcon.svelte";
  import ProgressRing from "./ProgressRing.svelte";
  import Tooltip from "./Tooltip.svelte";

  type Props = {
    onReauthenticate: () => Promise<void>;
    onSignOut: () => void;
  };

  let { onReauthenticate, onSignOut }: Props = $props();

  let isReauthenticating = $state(false);
  let isCancelled = $state(false);

  const handleReauthenticate = async () => {
    try {
      isReauthenticating = true;
      isCancelled = false;
      await onReauthenticate();
    } catch {
      isCancelled = true;
      setTimeout(() => (isCancelled = false), 4000);
    } finally {
      isReauthenticating = false;
    }
  };
</script>

<div class="flex flex-col gap-8">
  <div class="flex flex-col gap-4">
    <FeaturedIcon size="lg">
      <UserCheckIcon class="size-6" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h2 class="text-text-primary text-2xl font-medium">
        {$t`Session timed out`}
      </h2>
      <p class="text-text-tertiary text-base">
        <Trans>Sign in again to continue where you left off.</Trans>
      </p>
    </div>
  </div>

  <div class="flex flex-col gap-3">
    <Tooltip
      label={$t`Interaction canceled. Please try again.`}
      hidden={!isCancelled}
      manual
    >
      <button
        onclick={handleReauthenticate}
        class="btn w-full"
        disabled={isReauthenticating}
      >
        {#if isReauthenticating}
          <ProgressRing />
          <span>{$t`Signing in...`}</span>
        {:else}
          <span>{$t`Sign in`}</span>
        {/if}
      </button>
    </Tooltip>
    <button
      onclick={onSignOut}
      class="btn btn-tertiary w-full"
      disabled={isReauthenticating}
    >
      {$t`Cancel`}
    </button>
  </div>
</div>
