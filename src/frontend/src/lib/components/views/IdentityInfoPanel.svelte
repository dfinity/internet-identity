<script lang="ts">
  import Panel from "$lib/components/ui/Panel.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { InfoIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";

  interface Props {
    name: string;
  }

  const { name }: Props = $props();
</script>

<Panel>
  <div class="p-4">
    <h3 class="text-text-primary mb-2 text-lg font-semibold">My identity</h3>
    <h4 class="text-text-tertiary min-h-10 text-sm">
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
        Identity name
      </h5>
      <div class="flex items-center">
        <h5 class="text-text-primary text-sm font-semibold nth-[2]:hidden">
          {name}
        </h5>
      </div>
      <div class="flex items-center justify-center">
        <Tooltip
          label="Not editable"
          description="Your identity name is currently not editable. It is only ever visible to you."
          align="end"
          offset="0rem"
          class="!max-w-72"
          ><Button
            variant="tertiary"
            iconOnly
            class="!cursor-default !rounded-full"><InfoIcon size="20" /></Button
          ></Tooltip
        >
      </div>
    </div>
  </div>
</Panel>
