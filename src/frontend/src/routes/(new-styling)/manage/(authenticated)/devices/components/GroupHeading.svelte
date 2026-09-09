<script lang="ts">
  import {
    LaptopIcon,
    MonitorSmartphoneIcon,
    SmartphoneIcon,
    TabletIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
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
  <!-- "device(s)" only once there is more than one browser: two of them may be two
       machines or one machine twice, and nothing reported can tell those apart. A single
       browser needs no hedge. -->
  <h2 class="text-sm">
    {count === 1
      ? $t`1 browser on ${platform}`
      : $t`${count} browsers on ${platform} device(s)`}
  </h2>
</div>
