<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    // SSO display name from the well-known (e.g. "DFINITY ENTRA").
    name: string;
    // Runs the normal (primary-client) sign-in, then continues to the app.
    onContinue: () => void;
    onCancel: () => void;
    loading?: boolean;
  }

  const { name, onContinue, onCancel, loading = false }: Props = $props();
</script>

<div class="flex flex-1 flex-col">
  <div class="text-text-primary mb-8 flex w-full flex-col gap-5">
    <FeaturedIcon size="lg" variant="info">
      <SsoIcon class="size-5" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h1 class="text-2xl font-medium">{$t`First sign-in with ${name}`}</h1>
      <p class="text-text-tertiary text-base font-medium">
        {$t`Please sign in again to continue.`}
      </p>
    </div>
  </div>

  <div class="flex flex-col items-stretch gap-6">
    <button
      class="btn btn-primary btn-lg"
      type="button"
      disabled={loading}
      onclick={onContinue}
    >
      {#if loading}
        <ProgressRing />
        <span>{$t`Signing in...`}</span>
      {:else}
        <span>{$t`Continue`}</span>
      {/if}
    </button>
    <button
      type="button"
      onclick={onCancel}
      disabled={loading}
      class="text-text-secondary self-center text-sm font-semibold outline-0 hover:underline focus-visible:underline"
    >
      {$t`Cancel`}
    </button>
  </div>
</div>
