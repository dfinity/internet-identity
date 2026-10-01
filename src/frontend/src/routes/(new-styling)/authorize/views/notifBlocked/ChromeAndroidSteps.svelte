<script lang="ts">
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
      <Trans>Tap the highlighted <b>site settings</b> icon.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 pointer-events-none rounded-xl p-2.5"
    >
      <div class="text-text-primary flex items-center gap-3 pl-1.5">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-9 min-w-0 flex-1 items-center rounded-full pr-1 pl-2"
        >
          <div class="relative">
            <div
              class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-7 w-7 items-center justify-center rounded-full"
            >
              <svg
                viewBox="0 0 24 24"
                width="14"
                height="14"
                fill="none"
                stroke="currentColor"
                stroke-width="2"
                stroke-linecap="round"
                stroke-linejoin="round"
                class="shrink-0"
                ><path d="M20 7h-9"></path><path d="M14 17H5"></path><circle
                  cx="17"
                  cy="17"
                  r="3"
                ></circle><circle cx="7" cy="7" r="3"></circle></svg
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
        <svg
          viewBox="0 0 24 24"
          width="14"
          height="14"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          class="shrink-0"
          ><rect width="18" height="18" x="3" y="3" rx="2"></rect><text
            x="12"
            y="16.5"
            text-anchor="middle"
            font-size="12"
            font-weight="700"
            fill="currentColor"
            stroke="none"
            font-family="Inter, system-ui, sans-serif">2</text
          ></svg
        ><svg
          viewBox="0 0 24 24"
          width="14"
          height="14"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          class="mr-1 shrink-0"
          ><circle cx="12" cy="12" r="1"></circle><circle cx="12" cy="5" r="1"
          ></circle><circle cx="12" cy="19" r="1"></circle></svg
        >
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
        {$t`Open permissions`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Tap <b>Permissions</b>.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-3.5 pt-3 pb-3.5"
    >
      <div class="flex items-center justify-center px-0.5 pb-2">
        <span class="text-xs font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
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
          ><rect width="18" height="11" x="3" y="11" rx="2" ry="2"></rect><path
            d="M7 11V7a5 5 0 0 1 10 0v4"
          ></path></svg
        >
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">Connection is secure</span>
        </div>
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 my-1 h-px"
      ></div>
      <div class="relative mt-1.5 mb-1">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-10 items-center gap-2.5 rounded-full px-2.5"
        >
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
            ><line x1="21" x2="14" y1="4" y2="4"></line><line
              x1="10"
              x2="3"
              y1="4"
              y2="4"
            ></line><line x1="21" x2="12" y1="12" y2="12"></line><line
              x1="8"
              x2="3"
              y1="12"
              y2="12"
            ></line><line x1="21" x2="16" y1="20" y2="20"></line><line
              x1="12"
              x2="3"
              y1="20"
              y2="20"
            ></line><line x1="14" x2="14" y1="2" y2="6"></line><line
              x1="8"
              x2="8"
              y1="10"
              y2="14"
            ></line><line x1="16" x2="16" y1="18" y2="22"></line></svg
          >
          <div class="flex min-w-0 flex-1 flex-col">
            <span class="text-[11px]">Permissions</span><span
              class="text-text-tertiary text-[10px]">Notifications blocked</span
            >
          </div>
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
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 my-1 h-px"
      ></div>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
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
          ><path d="M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"
          ></path><path d="M3 3v5h5"></path><path d="M12 7v5l4 2"></path></svg
        >
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">Last visited yesterday</span>
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
        3
      </div>
      <h2 class="text-text-primary m-0 text-lg leading-7 font-normal">
        {$t`Open notification settings`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Tap <b>Notifications</b>.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-3.5 pt-3 pb-3.5"
    >
      <div class="flex items-center justify-center px-0.5 pb-2">
        <span class="text-xs font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 px-0.5 pt-1 pb-2">
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
          ><path d="m12 19-7-7 7-7"></path><path d="M19 12H5"></path></svg
        ><span class="text-[11px]">Permissions</span>
      </div>
      <div class="flex items-center gap-2.5 px-0.5 py-2">
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
          ><path d="M10.268 21a2 2 0 0 0 3.464 0"></path><path
            d="M17 17H4a1 1 0 0 1-.74-1.673C4.59 13.956 6 12.499 6 8a6 6 0 0 1 .258-1.742"
          ></path><path d="m2 2 20 20"></path><path
            d="M8.668 3.01A6 6 0 0 1 18 8c0 2.687.77 4.653 1.707 6.05"
          ></path></svg
        >
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">Notifications</span><span
            class="text-text-tertiary text-[10px]">Not allowed</span
          >
        </div>
        <div class="relative mr-1.5">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-6 w-6 items-center justify-center rounded-full"
          >
            <svg
              viewBox="0 0 24 24"
              width="12"
              height="12"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              class="shrink-0"
              ><path d="M15 3h6v6"></path><path d="M10 14 21 3"></path><path
                d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h6"
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
        4
      </div>
      <h2 class="text-text-primary m-0 text-lg leading-7 font-normal">
        {$t`Allow notifications`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Turn on <b>Show notifications</b>.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-3.5 pt-3 pb-3.5"
    >
      <div class="flex items-center gap-2.5 px-0.5 pb-2.5">
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
          ><path d="m12 19-7-7 7-7"></path><path d="M19 12H5"></path></svg
        ><span class="text-xs font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 pt-2 pr-2 pb-2 pl-0.5">
        <span class="flex-1 text-[11px]">Show notifications</span>
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
