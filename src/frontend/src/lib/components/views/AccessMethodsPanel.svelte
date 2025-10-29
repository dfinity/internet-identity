<script lang="ts">
  import Panel from "$lib/components/ui/Panel.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import { ChevronRightIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
</script>

<Panel>
  <div class="p-4">
    <h3 class="text-text-primary mb-2 text-lg font-semibold">Access methods</h3>
    <h4 class="text-text-tertiary min-h-10 text-sm">
      See how many access methods are within your identity and activate your
      recovery seed phrase.
    </h4>
  </div>
  <div class="grid grid-cols-[1fr_2fr_min-content]">
    <div
      class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t py-1.5 ps-4 pe-1.5"
    >
      <h5
        class="text-text-tertiary flex min-w-30 items-center pr-2 text-sm md:pr-4"
      >
        Total methods
      </h5>
      <div class="flex items-center">
        <h5 class="text-text-primary text-sm font-semibold nth-[2]:hidden">
          {identityInfo.totalAccessMethods}
        </h5>
      </div>
      <div class="flex items-center justify-center">
        <Button
          href="/manage/access"
          variant="tertiary"
          iconOnly
          class="!rounded-full"
        >
          <ChevronRightIcon class="size-5" />
        </Button>
      </div>
    </div>
  </div>
</Panel>
