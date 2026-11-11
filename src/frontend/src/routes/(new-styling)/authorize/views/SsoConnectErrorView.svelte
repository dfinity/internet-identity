<script lang="ts">
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { formatDuration, t } from "$lib/stores/locale.store";
  import { ssoErrorMessage } from "$lib/components/wizards/auth";
  import { DomainNotConfiguredError } from "$lib/utils/ssoDiscovery";

  interface Props {
    error: unknown;
    domain: string;
    onRetry: () => void;
  }

  const { error, domain, onRetry }: Props = $props();

  const retryAfter = $derived(
    error instanceof DomainNotConfiguredError && error.reason === "failed"
      ? error.retryAfter
      : undefined,
  );

  let now = $state(Date.now());

  $effect(() => {
    if (retryAfter === undefined) return;
    const timer = setInterval(() => (now = Date.now()), 1000);
    return () => clearInterval(timer);
  });

  const secondsLeft = $derived(
    retryAfter === undefined
      ? 0
      : Math.max(0, Math.ceil((retryAfter.getTime() - now) / 1000)),
  );
</script>

<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-100">
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Couldn't connect to your organization`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base font-medium text-pretty">
      {ssoErrorMessage(error, domain)}
    </p>
    <div class="flex flex-col gap-3">
      {#if retryAfter !== undefined}
        <button
          class="btn btn-primary"
          disabled={secondsLeft > 0}
          onclick={onRetry}
        >
          {#if secondsLeft > 0}
            {@const duration = $formatDuration(
              secondsLeft >= 60
                ? { minute: Math.ceil(secondsLeft / 60) }
                : { second: secondsLeft },
            )}
            {$t`Try again in ${duration}`}
          {:else}
            {$t`Try again`}
          {/if}
        </button>
      {/if}
      <button class="btn btn-secondary" onclick={() => window.close()}>
        <RotateCcwIcon class="size-4" />
        <span>{$t`Return to app`}</span>
      </button>
    </div>
  </AuthPanel>
</div>
