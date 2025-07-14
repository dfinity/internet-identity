<script lang="ts">
  import Panel from "$lib/components/ui/Panel.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { InfoIcon } from "@lucide/svelte";
  import PlaceHolder from "$lib/components/ui/PlaceHolder.svelte";
  import { fade } from "svelte/transition";
</script>

<Panel>
  <div class="p-4">
    <h3 class="text-text-primary mb-2 text-lg font-semibold">My Identity</h3>
    <h4 class="text-text-tertiary text-sm">
      Internet Identity is used to sign in securely and connect to apps with
      passkeys.
    </h4>
  </div>
  <div class="grid grid-cols-[1fr_2fr_min-content]">
    <div
      class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t px-4 py-4"
    >
      <h5
        class="text-text-tertiary flex min-w-30 items-center pr-2 text-sm md:pr-4"
      >
        Identity Name
      </h5>
      <div class="flex items-center">
        {#if identityInfo.name}
          <h5
            class="text-text-primary text-sm font-semibold nth-[2]:hidden"
            transition:fade={{ delay: 30 }}
          >
            {identityInfo.name}
          </h5>
        {:else}
          <PlaceHolder class="mr-8 h-4 w-full !rounded-sm" />
        {/if}
      </div>
      <div class="flex items-center justify-center">
        <Tooltip
          label="Your Identity name is currently not editable. It is only ever visible to you."
          ><InfoIcon
            size="20"
            class="text-text-primary stroke-fg-tertiary"
          /></Tooltip
        >
      </div>
    </div>
  </div>
</Panel>
