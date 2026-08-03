<script lang="ts">
  import { DownloadIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import {
    pwaInstallable,
    promptInstall,
    isStandaloneDisplay,
  } from "$lib/utils/pwaInstall";

  const titleId = $props.id();

  const alreadyInstalled = isStandaloneDisplay();
</script>

{#if $pwaInstallable && !alreadyInstalled}
  <section
    class="border-border-secondary bg-bg-secondary flex flex-row items-start gap-3 rounded-xl border p-4 sm:gap-4 sm:p-5"
  >
    <span
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <DownloadIcon class="size-5" />
    </span>

    <div class="flex min-w-0 flex-1 flex-col gap-1">
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`Install Internet Identity as an app`}
      </h3>
      <p class="text-text-tertiary text-sm">
        {$t`Required to receive push notifications in the background on this device.`}
      </p>
    </div>

    <button
      class="btn btn-primary btn-sm shrink-0"
      onclick={() => void promptInstall()}
      aria-labelledby={titleId}
    >
      {$t`Install`}
    </button>
  </section>
{/if}
