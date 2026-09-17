<script lang="ts">
  import {
    LaptopIcon,
    MonitorSmartphoneIcon,
    SmartphoneIcon,
    TabletIcon,
  } from "@lucide/svelte";
  import { Trans } from "$lib/components/locale";
  import { plural } from "$lib/stores/locale.store";
  import type { DeviceKind } from "../browsers";

  interface Props {
    kind: DeviceKind;
    /** What the owner calls the platform: "Mac", "iPhone", "Windows". */
    platform: string;
    count: number;
  }

  const { kind, platform, count }: Props = $props();
</script>

<div class="text-text-tertiary flex flex-row items-center gap-2 px-4 pt-4 pb-1">
  <span
    class="flex size-4 shrink-0 items-center justify-center"
    aria-hidden="true"
  >
    {#if kind === "laptop"}
      <LaptopIcon class="size-4" />
    {:else if kind === "phone"}
      <SmartphoneIcon class="size-4" />
    {:else if kind === "tablet"}
      <TabletIcon class="size-4" />
    {:else}
      <MonitorSmartphoneIcon class="size-4" />
    {/if}
  </span>
  <h2 class="text-sm">
    <Trans
      context="Browsers are grouped by platform, which cannot tell one device from several, so the plural hedges with device(s) instead of naming a number"
    >
      {$plural(count, {
        one: `# browser on ${platform}`,
        other: `# browsers on ${platform} device(s)`,
      })}
    </Trans>
  </h2>
</div>
