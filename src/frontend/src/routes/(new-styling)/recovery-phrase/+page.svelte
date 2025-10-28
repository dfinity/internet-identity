<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
</script>

<AuthPanel class="sm:max-w-140">
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
          <div class="bg-bg-primary">
            <label class="flex h-10 flex-row items-stretch">
              <!-- Left slot -->
              <span
                class="text-text-secondary border-border-secondary focus:border-border-brand flex h-full w-12 items-center rounded-l-full border-t border-b border-l px-3 text-center text-sm font-semibold"
              >
                {String(i + 1).padStart(2, "0")}
              </span>

              <!-- Text input -->
              <input
                type="text"
                id={`recovery-phrase-${i}`}
                class="text-text-primary ring-border-secondary focus:ring-border-brand w-full rounded-r-full border-none bg-transparent text-base opacity-100 ring outline-none ring-inset focus:ring-2"
              />
            </label>
          </div>
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
