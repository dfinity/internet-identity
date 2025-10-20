<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import ShieldIllustration from "$lib/components/illustrations/ShieldIllustration.svelte";
  import { SUPPORT_URL } from "$lib/config";
  import { onMount } from "svelte";
  import type { PageData } from "../$types";
  import Header from "$lib/components/layout/Header.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  const { data }: { data: PageData } = $props();

  const COUNTDOWN_SECONDS = 5;
  let countdown = $state(COUNTDOWN_SECONDS);
  let intervalId: number | undefined;
  let redirected = $state(false);

  const handleRedirect = () => {
    if (redirected || !data.redirectUrl) return;
    redirected = true;
    if (intervalId) window.clearInterval(intervalId);
    window.location.href = data.redirectUrl;
  };

  onMount(() => {
    // Clean up noRedirect parameter from URL if present
    if (data.noRedirect) {
      const url = new URL(window.location.href);
      url.searchParams.delete("noRedirect");
      window.history.replaceState({}, "", url.toString());
    }

    if (data.redirectUrl && !data.noRedirect) {
      intervalId = window.setInterval(() => {
        countdown -= 1;
        if (countdown <= 0) handleRedirect();
      }, 1000);

      return () => {
        if (intervalId) window.clearInterval(intervalId);
      };
    }
  });
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>

  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <div
      class="flex flex-1 flex-col items-stretch justify-end p-4 sm:max-w-100 sm:items-center sm:justify-center"
    >
      <div class="mb-8 flex flex-1 flex-col justify-center gap-2 sm:flex-none">
        {#if data.noRedirect}
          <h1 class="text-text-primary mb-3 text-center text-2xl font-medium">
            Unsupported Browser
          </h1>
          <p class="text-text-primary mb-4 text-center text-lg font-semibold">
            To continue:
          </p>
          <ol
            class="text-md text-text-tertiary mb-4 list-decimal space-y-2 pl-6 text-left font-medium"
          >
            <li>
              Tap the <strong>⋯</strong> or
              <strong>long-press the link that opened this page</strong>.
            </li>
            <li>
              Select "<strong>Open in Safari</strong>" or "<strong
                >Open in Chrome</strong
              >."
            </li>
            <li>Sign in there.</li>
          </ol>
          <p class="text-center">
            <a
              href={SUPPORT_URL}
              target="_blank"
              rel="noopener noreferrer"
              class="text-text-primary underline hover:no-underline"
              >Learn how to open links in your browser →</a
            >
          </p>
        {:else if data.redirectUrl}
          <div class="flex flex-col items-center justify-center gap-4">
            <ProgressRing class="text-fg-primary size-14" />
            <p
              class="text-md text-text-tertiary text-center font-medium text-balance"
            >
              You are being redirected to the app <span
                class="text-text-primary">{data.redirectUrl}</span
              > for signin.
            </p>
          </div>
        {:else}
          <h1 class="text-text-primary mb-3 text-center text-2xl font-medium">
            Browser Not Supported
          </h1>
          <p
            class="text-md text-text-tertiary text-center font-medium text-balance"
          >
            Your browser is not supported. Please visit our <a
              href={SUPPORT_URL}
              target="_blank"
              rel="noopener noreferrer"
              class="text-text-primary underline hover:no-underline"
              >support page</a
            > for more information and try again with a different browser.
          </p>
        {/if}
      </div>
      {#if data.redirectUrl && !data.noRedirect}
        <Button variant="primary" onclick={handleRedirect}>
          Go to the app {#if countdown >= 0}
            - (<span class="tabular-nums">{countdown}</span>){/if}
        </Button>
      {/if}
    </div>
  </div>
</div>
