<script lang="ts">
  import {
    availableBrowserLocale,
    locales,
    localeStore,
  } from "$lib/stores/locale.store";
  import Button from "$lib/components/ui/Button.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { ChevronDownIcon } from "@lucide/svelte";

  let buttonRef = $state<HTMLElement>();
  let isOpen = $state(false);
</script>

{#if $locales.length > 1}
  <Button
    bind:element={buttonRef}
    onclick={() => (isOpen = true)}
    variant="tertiary"
    class="uppercase"
  >
    {$localeStore}
    <ChevronDownIcon class="size-5" />
  </Button>
{/if}
{#if isOpen}
  <Popover
    anchor={buttonRef}
    onClose={() => (isOpen = false)}
    direction="down"
    align="start"
    distance="0.25rem"
    responsive={false}
    class="!w-18 !p-1.5"
  >
    <ul class="flex flex-col">
      {#each $locales as locale}
        <li class="contents">
          <Button
            onclick={() => {
              isOpen = false;
              // Switch back to locale auto-detection if locale matches browser
              if (locale === availableBrowserLocale) {
                localeStore.reset();
              } else {
                localeStore.set(locale);
              }
            }}
            variant="tertiary"
            class={[
              "justify-start text-start uppercase",
              locale === $localeStore &&
                "[ul:not(:hover)_&]:bg-bg-primary_hover",
            ]}
          >
            {locale}
          </Button>
        </li>
      {/each}
    </ul>
  </Popover>
{/if}
