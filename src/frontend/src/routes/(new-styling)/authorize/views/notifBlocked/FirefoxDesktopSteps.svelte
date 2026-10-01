<script lang="ts">
  import Settings2Icon from "@lucide/svelte/icons/settings-2";
  import ShieldCheckIcon from "@lucide/svelte/icons/shield-check";
  import XIcon from "@lucide/svelte/icons/x";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  // Ported from the design, markup and all, so the steps are what was drawn rather
  // than a reading of it. Only Internet Identity's own words go through `$t`: the
  // labels inside a mock are the browser's own, and inventing translations for them
  // would put words in its mouth.
  const { host, address }: { host: string; address: string } = $props();
</script>

<ol class="flex flex-col gap-3">
  <li
    class="bg-surface-light-200 dark:bg-surface-dark-700 rounded-xl px-3 pt-5 pb-4"
  >
    <div class="mb-3 flex flex-row items-center gap-3">
      <div
        aria-hidden="true"
        class="bg-fg-primary-inversed text-fg-secondary h-8 w-8 shrink-0 rounded-full text-center text-base leading-8"
      >
        1
      </div>
      <h2 class="text-text-primary m-0 text-lg leading-7 font-normal">
        {$t`Open site permissions`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Select the highlighted <b>permissions</b> icon.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 pointer-events-none rounded-xl p-2.5"
    >
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 text-text-primary flex h-9 items-center rounded-full pr-3 pl-2.5"
      >
        <ShieldCheckIcon class="size-3.5 shrink-0" />
        <div class="relative ml-2">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-7 items-center gap-1.5 rounded-full px-2"
          >
            <Settings2Icon class="size-[13px] shrink-0" /><svg
              viewBox="0 0 24 24"
              width="13"
              height="13"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              class="shrink-0"
              ><path
                d="M19 19H6.828a2 2 0 0 0-1.414.586l-2.202 2.202A.71.71 0 0 1 2 21.286V5a2 2 0 0 1 1.184-1.826"
              ></path><path d="m2 2 20 20"></path><path
                d="M8.656 3H20a2 2 0 0 1 2 2v11.344"
              ></path></svg
            >
          </div>
          <div
            class="absolute -inset-[5px] rounded-full border border-blue-700 dark:border-blue-300"
          ></div>
          <div
            class="absolute -inset-[10px] rounded-full border-[0.75px] border-blue-500 dark:border-blue-500"
          ></div>
          <div
            class="absolute -inset-[14px] rounded-full border-[0.5px] border-blue-300 dark:border-blue-700"
          ></div>
          <div
            class="absolute -inset-[18px] rounded-full border-[0.25px] border-blue-200 dark:border-blue-800"
          ></div>
        </div>
        <span class="ml-5 text-[10px] whitespace-nowrap">{address}</span>
      </div>
    </div>
  </li>
  <li
    class="bg-surface-light-200 dark:bg-surface-dark-700 rounded-xl px-3 pt-5 pb-4"
  >
    <div class="mb-3 flex flex-row items-center gap-3">
      <div
        aria-hidden="true"
        class="bg-fg-primary-inversed text-fg-secondary h-8 w-8 shrink-0 rounded-full text-center text-base leading-8"
      >
        2
      </div>
      <h2 class="text-text-primary m-0 text-lg leading-7 font-normal">
        {$t`Clear the block`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Select <b>Blocked ×</b> to clear it.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-3.5 pt-3 pb-3.5"
    >
      <div class="px-0.5 pb-2.5 text-center text-[12px] font-semibold">
        Permissions for {host}
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 mb-1 h-px"
      ></div>
      <div class="flex items-center gap-2.5 px-0.5 py-2.5">
        <svg
          viewBox="0 0 24 24"
          width="13"
          height="13"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          class="shrink-0"
          ><path
            d="M19 19H6.828a2 2 0 0 0-1.414.586l-2.202 2.202A.71.71 0 0 1 2 21.286V5a2 2 0 0 1 1.184-1.826"
          ></path><path d="m2 2 20 20"></path><path
            d="M8.656 3H20a2 2 0 0 1 2 2v11.344"
          ></path></svg
        ><span class="flex-1 text-[11px]">Send notifications</span>
        <div class="relative">
          <div
            class="border-surface-light-300 dark:border-surface-dark-600 flex h-[22px] items-center gap-1 rounded-full border pr-1.5 pl-[9px] text-[10px]"
          >
            Blocked<XIcon class="size-2.5 shrink-0" />
          </div>
          <div
            class="absolute -inset-[5px] rounded-full border border-blue-700 dark:border-blue-300"
          ></div>
          <div
            class="absolute -inset-[10px] rounded-full border-[0.75px] border-blue-500 dark:border-blue-500"
          ></div>
          <div
            class="absolute -inset-[14px] rounded-full border-[0.5px] border-blue-300 dark:border-blue-700"
          ></div>
          <div
            class="absolute -inset-[18px] rounded-full border-[0.25px] border-blue-200 dark:border-blue-800"
          ></div>
        </div>
      </div>
    </div>
  </li>
</ol>
