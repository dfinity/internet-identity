<script lang="ts">
  import Settings2Icon from "@lucide/svelte/icons/settings-2";
  import ArrowLeftIcon from "@lucide/svelte/icons/arrow-left";
  import ArrowRightIcon from "@lucide/svelte/icons/arrow-right";
  import BellIcon from "@lucide/svelte/icons/bell";
  import ChevronRightIcon from "@lucide/svelte/icons/chevron-right";
  import LockIcon from "@lucide/svelte/icons/lock";
  import RotateCwIcon from "@lucide/svelte/icons/rotate-cw";
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
        {$t`Open site settings`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Select the highlighted <b>site settings</b> icon.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 pointer-events-none rounded-xl p-2.5"
    >
      <div class="text-text-primary flex items-center gap-3 pl-1.5">
        <ArrowLeftIcon class="size-3.5 shrink-0" /><ArrowRightIcon
          class="size-3.5 shrink-0 opacity-40"
        /><RotateCwIcon class="size-3.5 shrink-0" />
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-9 min-w-0 flex-1 items-center rounded-full pr-1 pl-2"
        >
          <div class="relative">
            <div
              class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-7 w-7 items-center justify-center rounded-full"
            >
              <Settings2Icon class="size-3.5 shrink-0" />
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
        {$t`Allow notifications`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Turn on <b>Notifications</b>.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-3.5 pt-3 pb-3.5"
    >
      <div class="flex items-center justify-between px-0.5 pb-2">
        <span class="text-[12px] font-semibold">{host}</span><XIcon
          class="size-3 shrink-0"
        />
      </div>
      <div class="flex items-center gap-2.5 pt-2 pr-2 pb-2 pl-0.5 opacity-45">
        <LockIcon class="size-[13px] shrink-0" /><span
          class="flex-1 text-[11px]">Connection is secure</span
        ><ChevronRightIcon class="size-3 shrink-0" />
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 my-1 h-px"
      ></div>
      <div class="flex items-center gap-2.5 pt-2 pr-2 pb-2 pl-0.5">
        <BellIcon class="size-[13px] shrink-0" /><span
          class="flex-1 text-[11px]">Notifications</span
        >
        <div class="relative">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 xb-track relative h-4 w-7 rounded-full"
          >
            <div
              class="bg-text-primary xb-knob absolute top-0.5 left-0.5 h-3 w-3 rounded-full"
            ></div>
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
