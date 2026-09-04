<script lang="ts">
  import { ChevronDownIcon } from "@lucide/svelte";
  import type { BrowserKind } from "$lib/utils/notifications/notificationDiagnostics";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    /** Which browser's steps to show; from the stored diagnostics. */
    browser: BrowserKind;
    /** Start expanded (settings) or collapsed behind a summary (opt-in). */
    open?: boolean;
  }

  const { browser, open = false }: Props = $props();

  const steps = $derived.by((): string[] => {
    switch (browser) {
      case "firefox":
        return [
          $t`Click the lock icon in the address bar.`,
          $t`Under Permissions, clear the "Blocked" setting for Send Notifications.`,
          $t`Reload the page and try again.`,
        ];
      case "safari":
        return [
          $t`Open Safari, then Settings, then Websites, then Notifications.`,
          $t`Find id.ai in the list and set it to Allow.`,
          $t`Return to this page and try again.`,
        ];
      case "android":
        return [
          $t`Tap the site-info icon to the left of the address bar.`,
          $t`Open Permissions, then Notifications, then Allow.`,
          $t`Reload the page and turn notifications on again.`,
        ];
      case "chrome":
      case "edge":
        return [
          $t`Click the tune or lock icon at the left of the address bar.`,
          $t`Find Notifications and switch it to Allow.`,
          $t`Reload the page and turn notifications on again.`,
        ];
      default:
        return [
          $t`Open your browser's site settings for this page.`,
          $t`Allow notifications for id.ai.`,
          $t`Reload the page and try again.`,
        ];
    }
  });
</script>

<details class="group border-border-tertiary rounded-xl border" {open}>
  <summary
    class="text-text-secondary flex cursor-pointer list-none items-center justify-between gap-2 px-4 py-3 text-sm font-medium"
  >
    {$t`How to unblock`}
    <ChevronDownIcon
      class="size-4 transition-transform group-open:rotate-180"
      aria-hidden="true"
    />
  </summary>
  <div class="border-border-tertiary border-t px-4 py-3">
    {#if browser === "ios"}
      <p class="text-text-tertiary text-sm">
        <Trans>
          On iPhone and iPad, notifications work only when the site is added to
          the Home Screen. Open Internet Identity in Safari, tap Share, then Add
          to Home Screen, and open it from there to enable notifications.
        </Trans>
      </p>
    {:else}
      <ol class="text-text-tertiary flex flex-col gap-2 text-sm">
        {#each steps as step, index (index)}
          <li class="flex gap-2.5">
            <span
              class="bg-bg-secondary text-text-secondary flex size-5 shrink-0 items-center justify-center rounded-full text-xs font-semibold"
            >
              {index + 1}
            </span>
            <span>{step}</span>
          </li>
        {/each}
      </ol>
    {/if}
  </div>
</details>
