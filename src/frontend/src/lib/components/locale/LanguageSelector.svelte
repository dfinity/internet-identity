<script lang="ts">
  import {
    availableBrowserLocale,
    locales,
    localeStore,
  } from "$lib/stores/locale.store";
  import Button from "$lib/components/ui/Button.svelte";
  import { ChevronDownIcon } from "@lucide/svelte";
  import Select from "$lib/components/ui/Select.svelte";
</script>

{#if $locales.length > 1}
  <Select
    class="!w-18"
    options={$locales.map((locale) => ({
      value: locale,
      label: locale.toUpperCase(),
    }))}
    onChange={(value) => {
      // Switch back to locale auto-detection if locale matches browser
      if (value === availableBrowserLocale) {
        localeStore.reset();
      } else {
        localeStore.set(value);
      }
    }}
  >
    <Button variant="tertiary" class="uppercase">
      {$localeStore}
      <ChevronDownIcon class="size-5" />
    </Button>
  </Select>
{/if}
