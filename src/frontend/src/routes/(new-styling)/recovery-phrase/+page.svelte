<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
</script>

<div
  class="flex flex-1 flex-row items-end justify-center sm:max-w-120 sm:items-center"
>
  <AuthPanel>
    <div class="flex flex-col gap-6">
      <div class="flex flex-col gap-3">
        <h1 class="text-text-primary text-2xl font-medium">
          {$t`Secure recovery`}
        </h1>
        <p class="text-text-tertiary text-sm">
          {$t`Enter your recovery phrases in each box, spelled correctly and in order.`}
        </p>
      </div>
      <div class="flex flex-col gap-3">
        <div class="grid grid-cols-3 gap-3">
          {#each Array(24) as _, i}
            <label class="relative h-8">
              <!-- Text input -->
              <input
                type="text"
                id={`recovery-phrase-${i}`}
                class="peer text-text-primary ring-border-secondary focus:ring-border-brand h-8 w-full rounded-full border-none bg-transparent pl-10 text-base ring outline-none ring-inset focus:ring-2"
              />
              <!-- Left slot -->
              <!-- Reverse order to use "peer" class to change the border color when peer is focused -->
              <span
                class="peer peer-focus:border-border-brand text-text-secondary border-border-secondary absolute top-0 left-0 flex h-8 w-8 items-center border-r-1 px-2 text-center text-sm font-semibold peer-focus:border-r-2"
              >
                {String(i + 1).padStart(2, "0")}
              </span>
            </label>
          {/each}
        </div>
        <div class="flex flex-row gap-2">
          <Button class="w-full" variant="tertiary">{$t`Show all`}</Button>
          <Button class="w-full" variant="tertiary">{$t`Clear all`}</Button>
        </div>
      </div>
      <Button size="xl" variant="secondary">{$t`Cancel`}</Button>
    </div>
  </AuthPanel>
</div>
