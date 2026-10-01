<script lang="ts">
  import BellIcon from "@lucide/svelte/icons/bell";
  import ChevronsUpDownIcon from "@lucide/svelte/icons/chevrons-up-down";
  import GlobeIcon from "@lucide/svelte/icons/globe";
  import HandIcon from "@lucide/svelte/icons/hand";
  import ShieldHalfIcon from "@lucide/svelte/icons/shield-half";
  import UserRoundIcon from "@lucide/svelte/icons/user-round";
  import VideoIcon from "@lucide/svelte/icons/video";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  // Ported from the design, markup and all, so the steps are what was drawn rather
  // than a reading of it. Only Internet Identity's own words go through `$t`: the
  // labels inside a mock are the browser's own, and inventing translations for them
  // would put words in its mouth.
  const { host }: { host: string } = $props();
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
        {$t`Open Safari settings`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Select <b>Safari</b> › <b>Settings…</b> in the menu bar.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-2.5 pt-2 pb-3"
    >
      <div class="flex items-center gap-3 text-[10px]">
        <svg
          viewBox="0 0 24 24"
          width="11"
          height="11"
          fill="currentColor"
          class="ml-1 shrink-0"
          ><path
            d="M12.152 6.896c-.948 0-2.415-1.078-3.96-1.04-2.04.027-3.91 1.183-4.961 3.014-2.117 3.675-.546 9.103 1.519 12.09 1.013 1.454 2.208 3.09 3.792 3.039 1.52-.065 2.09-.987 3.935-.987 1.831 0 2.35.987 3.96.948 1.637-.026 2.676-1.48 3.676-2.948 1.156-1.688 1.636-3.325 1.662-3.415-.039-.013-3.182-1.221-3.22-4.857-.026-3.04 2.48-4.494 2.597-4.559-1.429-2.09-3.623-2.324-4.39-2.376-2-.156-3.675 1.09-4.61 1.09zM15.53 3.83c.843-1.012 1.4-2.427 1.245-3.83-1.207.052-2.662.805-3.532 1.818-.78.896-1.454 2.338-1.273 3.714 1.338.104 2.715-.688 3.559-1.701"
          ></path></svg
        ><span
          class="bg-surface-light-300 dark:bg-surface-dark-600 -mx-2 rounded-full px-2 py-0.5 font-bold"
          >Safari</span
        ><span>File</span><span>Edit</span><span>View</span><span>History</span
        ><span>Bookmarks</span>
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 mt-1 ml-[19px] w-[190px] rounded-[10px] px-[3px] py-[5px]"
      >
        <div class="px-[5px] py-1 text-[10px]">About Safari</div>
        <div class="px-[5px] py-1 text-[10px]">Safari Extensions…</div>
        <div
          class="bg-surface-light-300 dark:bg-surface-dark-600 mx-[5px] my-[3px] h-px"
        ></div>
        <div class="relative">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex items-center gap-1.5 rounded-[5px] px-[5px] py-1 text-[10px]"
          >
            <svg
              viewBox="0 0 24 24"
              width="11"
              height="11"
              fill="currentColor"
              class="shrink-0"
              ><path
                fill-rule="evenodd"
                d="M21.78 11.36L23.19 11.56L23.19 12.44L21.78 12.64L21.72 13.28L21.61 13.91L22.92 14.47L22.70 15.32L21.28 15.15L21.05 15.75L20.79 16.33L21.91 17.21L21.47 17.98L20.15 17.44L19.77 17.97L19.37 18.46L20.22 19.60L19.60 20.22L18.46 19.37L17.97 19.77L17.44 20.15L17.98 21.47L17.21 21.91L16.33 20.79L15.75 21.05L15.15 21.28L15.32 22.70L14.47 22.92L13.91 21.61L13.28 21.72L12.64 21.78L12.44 23.19L11.56 23.19L11.36 21.78L10.72 21.72L10.09 21.61L9.53 22.92L8.68 22.70L8.85 21.28L8.25 21.05L7.67 20.79L6.79 21.91L6.02 21.47L6.56 20.15L6.03 19.77L5.54 19.37L4.40 20.22L3.78 19.60L4.63 18.46L4.23 17.97L3.85 17.44L2.53 17.98L2.09 17.21L3.21 16.33L2.95 15.75L2.72 15.15L1.30 15.32L1.08 14.47L2.39 13.91L2.28 13.28L2.22 12.64L0.81 12.44L0.81 11.56L2.22 11.36L2.28 10.72L2.39 10.09L1.08 9.53L1.30 8.68L2.72 8.85L2.95 8.25L3.21 7.67L2.09 6.79L2.53 6.02L3.85 6.56L4.23 6.03L4.63 5.54L3.78 4.40L4.40 3.78L5.54 4.63L6.03 4.23L6.56 3.85L6.02 2.53L6.79 2.09L7.67 3.21L8.25 2.95L8.85 2.72L8.68 1.30L9.53 1.08L10.09 2.39L10.72 2.28L11.36 2.22L11.56 0.81L12.44 0.81L12.64 2.22L13.28 2.28L13.91 2.39L14.47 1.08L15.32 1.30L15.15 2.72L15.75 2.95L16.33 3.21L17.21 2.09L17.98 2.53L17.44 3.85L17.97 4.23L18.46 4.63L19.60 3.78L20.22 4.40L19.37 5.54L19.77 6.03L20.15 6.56L21.47 6.02L21.91 6.79L20.79 7.67L21.05 8.25L21.28 8.85L22.70 8.68L22.92 9.53L21.61 10.09L21.72 10.72ZM4.40 12a7.6 7.6 0 1 0 15.2 0a7.6 7.6 0 1 0 -15.2 0Z"
              ></path><path
                fill-rule="evenodd"
                d="M9.60 12a2.4 2.4 0 1 0 4.8 0a2.4 2.4 0 1 0 -4.8 0ZM11.00 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0Z"
              ></path><path
                d="M14.20 10.95L19.90 11.35L19.90 12.65L14.20 13.05ZM11.81 14.43L8.61 19.17L7.49 18.52L9.99 13.38ZM9.99 10.62L7.49 5.48L8.61 4.83L11.81 9.57Z"
              ></path></svg
            ><span class="flex-1">Settings…</span><span class="opacity-70"
              >⌘ ,</span
            >
          </div>
          <div
            class="absolute -inset-[5px] rounded-[10px] border border-blue-700 dark:border-blue-300"
          ></div>
          <div
            class="absolute -inset-[10px] rounded-[15px] border-[0.75px] border-blue-500 dark:border-blue-500"
          ></div>
          <div
            class="absolute -inset-[14px] rounded-[19px] border-[0.5px] border-blue-300 dark:border-blue-700"
          ></div>
          <div
            class="absolute -inset-[18px] rounded-[23px] border-[0.25px] border-blue-200 dark:border-blue-800"
          ></div>
        </div>
        <div class="flex items-center gap-1.5 px-[5px] py-1 text-[10px]">
          <ShieldHalfIcon class="size-[11px] shrink-0" />Privacy Report…
        </div>
        <div
          class="bg-surface-light-300 dark:bg-surface-dark-600 mx-[5px] my-[3px] h-px"
        ></div>
        <div class="px-[5px] py-1 text-[10px]">Clear History…</div>
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
        {$t`Go to Websites`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Select the <b>Websites</b> tab.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-3.5 py-2"
    >
      <div class="flex items-center justify-between px-2">
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
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
            ><path
              d="M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z"
            ></path><circle cx="12" cy="12" r="3"></circle></svg
          ><span>General</span>
        </div>
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
          <HandIcon class="size-3.5 shrink-0" /><span>Privacy</span>
        </div>
        <div class="relative">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex flex-col items-center gap-0.5 rounded-full px-3 py-1 text-[9px]"
          >
            <GlobeIcon class="size-3.5 shrink-0" /><span>Websites</span>
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
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
          <UserRoundIcon class="size-3.5 shrink-0" /><span>Profiles</span>
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
        {$t`Allow notifications`}
      </h2>
    </div>
    <p class="text-text-primary mb-5 text-sm leading-5">
      <Trans>Select <b>Notifications</b>, set {host} to <b>Allow</b>.</Trans>
    </p>
    <div
      aria-hidden="true"
      class="bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl px-3.5 pt-3 pb-3.5"
    >
      <div class="flex gap-2.5">
        <div class="flex w-24 shrink-0 flex-col gap-0.5 text-[10px]">
          <div class="flex items-center gap-1.5 px-1.5 py-[5px] opacity-45">
            <VideoIcon class="size-[11px] shrink-0" />Camera
          </div>
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex items-center gap-1.5 rounded-md px-1.5 py-[5px]"
          >
            <BellIcon class="size-[11px] shrink-0" />Notifications
          </div>
        </div>
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex min-w-0 flex-1 items-center rounded-lg pt-2 pr-2 pb-2 pl-2.5"
        >
          <span class="flex-1 text-[10px]">{host}</span>
          <div class="relative">
            <div
              class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-5 items-center gap-1 rounded-full pr-[5px] pl-2 text-[10px]"
            >
              <span class="inline-grid"
                ><span class="xb-out [grid-area:1/1]">Deny</span><span
                  class="xb-in opacity-0 [grid-area:1/1]">Allow</span
                ></span
              ><ChevronsUpDownIcon class="size-[9px] shrink-0" />
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
    </div>
  </li>
</ol>
