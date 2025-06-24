<script lang="ts">
  import { type DeviceWithUsage } from "$lib/generated/internet_identity_types";
  import { formatLastUsage } from "$lib/utils/time";
  import { nonNullish } from "@dfinity/utils";
  import { fade } from "svelte/transition";

  let { accessMethod }: { accessMethod: DeviceWithUsage } = $props();
</script>

<h5
  class="text-text-primary text-sm font-semibold nth-[2]:hidden"
  transition:fade={{ delay: 30 }}
>
  {accessMethod.alias}
  {#if nonNullish(accessMethod.last_usage[0])}
    <span class="text-text-tertiary ml-3 font-normal"
      >Last used {formatLastUsage(
        new Date(Number(accessMethod.last_usage[0] / BigInt(1000000))),
      )}</span
    >
  {/if}
</h5>
