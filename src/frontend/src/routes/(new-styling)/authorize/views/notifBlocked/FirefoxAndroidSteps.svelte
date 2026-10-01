<script lang="ts">
  import EllipsisVerticalIcon from "@lucide/svelte/icons/ellipsis-vertical";
  import LockIcon from "@lucide/svelte/icons/lock";
  import PlusIcon from "@lucide/svelte/icons/plus";
  import ShieldCheckIcon from "@lucide/svelte/icons/shield-check";
  import SquareIcon from "@lucide/svelte/icons/square";
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
      <Trans>Tap the highlighted <b>shield</b> icon.</Trans>
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
              <ShieldCheckIcon class="size-3.5 shrink-0" />
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
        <PlusIcon class="size-3.5 shrink-0" /><SquareIcon
          class="size-3.5 shrink-0"
        /><EllipsisVerticalIcon class="mr-1 size-3.5 shrink-0" />
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
      <Trans>Tap <b>Blocked</b> to allow.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-3 pt-2 pb-3.5"
    >
      <div
        class="bg-surface-light-300 dark:bg-surface-dark-600 mx-auto mb-2.5 h-[3px] w-7 rounded-full"
      ></div>
      <div class="flex items-center gap-2.5 px-0.5 pb-3">
        <div
          class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-6 w-6 shrink-0 items-center justify-center rounded-md"
        >
          <svg
            viewBox="0 0 46 22"
            width="16"
            height="8"
            fill="currentColor"
            class="shrink-0"
            ><path
              d="M45.9992 11C45.9992 4.93463 40.9792 0 34.8188 0C32.2505 0 29.4539 1.33375 26.4911 3.96324C25.0903 5.2064 23.8757 6.5361 22.9637 7.60537C19.7613 3.99235 15.4664 0 11.1804 0C5.99986 0 1.4822 3.63324 0.306661 8.44735C0.308259 8.44331 0.309856 8.43765 0.311453 8.43199C0.309856 8.43765 0.308259 8.4425 0.306661 8.44735C0.106213 9.2675 0 10.1224 0 11C0 17.0654 4.94012 22 11.1005 22C13.6688 22 16.5453 20.6663 19.5081 18.0368C20.9089 16.7936 22.1235 15.4639 23.0355 14.3946C26.2379 18.0076 30.5336 22 34.8196 22C40.0001 22 44.5178 18.3668 45.6933 13.5535C45.8938 12.7333 46 11.8784 46 11.0008L45.9992 11ZM23.823 7.59404C24.8699 6.39779 25.9081 5.34147 26.9127 4.45015C29.7557 1.92662 32.4159 0.647059 34.8188 0.647059C40.6318 0.647059 45.3603 5.29132 45.3603 11C45.3603 11.8007 45.2653 12.6007 45.0768 13.3788C45.0465 13.4685 44.6592 14.5426 43.5555 15.5868C42.122 16.9432 40.179 17.6315 37.78 17.6324C40.3603 16.4968 42.1651 13.9457 42.1651 11C42.1651 6.99551 38.8693 3.73838 34.8188 3.73838C33.24 3.73838 31.3074 4.74375 29.0721 6.72779C28.0667 7.62074 27.0485 8.67059 25.9736 9.92507L25.5519 10.4152L23.3917 8.07529L23.823 7.59485V7.59404ZM20.0017 11.1084C19.1488 12.134 17.9189 13.5316 16.5054 14.7861C13.8716 17.1244 12.1594 17.6146 11.1804 17.6146C7.48605 17.6146 4.47294 14.647 4.47294 11C4.47294 7.35301 7.48286 4.40809 11.1804 4.38544C11.3145 4.38544 11.4766 4.39919 11.6731 4.43478C13.5785 5.17485 15.2804 6.35574 16.3082 7.30853C17.1347 8.07529 18.6321 9.64441 20.0009 11.1076L20.0017 11.1084ZM22.1762 14.406C21.1285 15.6022 20.0911 16.6585 19.0865 17.5499C16.2834 20.0378 13.5218 21.3529 11.1005 21.3529C8.29343 21.3529 5.66126 20.2732 3.68712 18.3126C1.72098 16.3601 0.638878 13.7629 0.638878 11C0.638878 10.1993 0.73471 9.39853 0.92238 8.62125C0.954324 8.52824 1.34164 7.45574 2.44371 6.41316C3.87719 5.05677 5.82018 4.36846 8.21916 4.36765C5.6389 5.50324 3.83407 8.05427 3.83407 11C3.83407 15.0045 7.12988 18.2616 11.1804 18.2616C12.7592 18.2616 14.6918 17.2563 16.9271 15.2722C17.9325 14.3793 18.9507 13.3294 20.0256 12.0749L20.4465 11.584C20.4465 11.584 22.5747 13.8891 22.5979 13.915L22.1754 14.406H22.1762ZM25.9975 10.8916C26.8504 9.86603 28.0803 8.46838 29.4938 7.2139C32.1276 4.87559 33.8398 4.38544 34.8188 4.38544C38.5131 4.38544 41.5263 7.35301 41.5263 11C41.5263 14.647 38.5163 17.5919 34.8188 17.6146C34.6847 17.6146 34.5226 17.6008 34.3253 17.5652C34.3269 17.5652 34.3277 17.566 34.3293 17.5668C32.4222 16.8268 30.7188 15.6451 29.6902 14.6915C28.8637 13.9247 27.3663 12.3556 25.9967 10.8924L25.9975 10.8916ZM45.6885 13.5648C45.6901 13.5607 45.6909 13.5559 45.6925 13.5526C45.6917 13.5559 45.6901 13.5607 45.6885 13.5648Z"
            ></path></svg
          >
        </div>
        <div class="flex flex-col">
          <span class="text-[11px] font-semibold">Internet Identity</span><span
            class="text-text-tertiary text-[10px]">{host}</span
          >
        </div>
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 flex items-center gap-2.5 rounded-xl px-2.5 py-2 opacity-45"
      >
        <LockIcon class="size-[13px] shrink-0" /><span
          class="flex-1 text-[11px]">Secure connection</span
        >
      </div>
      <div class="px-0.5 pt-3 pb-1.5 text-[10px] font-semibold">
        Permissions
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 flex items-center gap-2.5 rounded-xl px-2.5 py-2.5"
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
          ><path
            d="M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z"
          ></path><path d="M13 8H7"></path><path d="M17 12H7"></path></svg
        ><span class="flex-1 text-[11px]">Notification</span>
        <div class="relative mr-1">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 rounded-full px-1.5 py-0.5 text-[10px]"
          >
            <span class="inline-grid"
              ><span class="xb-out [grid-area:1/1]">Blocked</span><span
                class="xb-in opacity-0 [grid-area:1/1]">Allowed</span
              ></span
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
</ol>
