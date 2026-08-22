<script lang="ts">
  import { BellIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    /** dApp name for the heading, or undefined when it isn't known. */
    appName: string | undefined;
    /** Runs the opt-in; resolves once the caller is ready to continue. */
    onAllow: () => Promise<void>;
    /** Continues sign-in without enabling notifications. */
    onSkip: () => void;
  }

  const { appName, onAllow, onSkip }: Props = $props();

  let busy = $state(false);

  const handleAllow = async (): Promise<void> => {
    busy = true;
    try {
      await onAllow();
    } finally {
      busy = false;
    }
  };
</script>

<div
  class="flex flex-1 flex-col items-stretch justify-end p-4 sm:max-w-100 sm:items-center sm:justify-center"
>
  <div
    class="text-text-primary flex h-50 flex-1 items-center justify-center sm:flex-none"
  >
    <BellIcon class="size-16" aria-hidden="true" />
  </div>
  <div class="mb-8 flex flex-col gap-2">
    <h1 class="text-text-primary mb-3 text-center text-2xl font-medium">
      {appName !== undefined
        ? $t`Get notified by ${appName}`
        : $t`Turn on notifications`}
    </h1>
    <p
      class="text-text-tertiary text-center text-base font-medium text-balance"
    >
      <Trans>
        Let this app send notifications to this device. You can turn them off
        anytime in settings.
      </Trans>
    </p>
  </div>
  <div class="flex flex-col gap-3">
    <button class="btn btn-primary" onclick={handleAllow} disabled={busy}>
      {busy ? $t`Setting up…` : $t`Allow notifications`}
    </button>
    <button class="btn btn-tertiary" onclick={onSkip} disabled={busy}>
      {$t`Not now`}
    </button>
  </div>
</div>
