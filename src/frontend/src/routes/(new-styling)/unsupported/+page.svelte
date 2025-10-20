<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import ShieldIllustration from "$lib/components/illustrations/ShieldIllustration.svelte";
  import { SUPPORT_URL } from "$lib/config";
  import { onMount } from "svelte";
  import type { PageData } from "../$types";
  import Header from "$lib/components/layout/Header.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { isNullish, nonNullish } from "@dfinity/utils";

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
      <div class="mb-8 flex flex-1 flex-col justify-center sm:flex-none">
        {#if nonNullish(data.noRedirect) || isNullish(data.redirectUrl)}
          <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
            <TriangleAlertIcon size="1.25rem" class="text-fg-warning-primary" />
          </FeaturedIcon>
          <h1
            class="text-text-primary mb-3 text-center text-2xl font-medium sm:text-left"
          >
            {$t`Unsupported Browser`}
          </h1>
          <p class="text-text-primary mb-5 text-center sm:text-left">
            {$t`It looks like you’re trying to sign in from inside an app (such as X, Telegram, or Instagram). This app's built-in browser is not supported.`}
          </p>
          <div>
            <p class="text-text-primary mb-3 text-center sm:text-left">
              <strong>{$t`To continue:`}</strong>
            </p>
            <ol
              class="text-md text-text-tertiary mb-5 list-decimal space-y-2 pl-6 text-left font-medium"
            >
              {#if nonNullish(data.redirectUrl)}
                <li>
                  {$t`Long-press the link that opened this page.`}
                </li>
              {:else}
                <li>
                  <Trans>
                    Tap the <span class="font-semibold">⋯</span> (three dots) or
                    long-press the link that opened this page.
                  </Trans>
                </li>
              {/if}
              <li>
                <Trans>
                  Select <span class="font-semibold">Open in Safari</span> or
                  <span class="font-semibold">Open in Chrome</span>.
                </Trans>
              </li>
              <li>{$t`Sign in there.`}</li>
            </ol>
          </div>
          <p class="text-center">
            <a
              href={SUPPORT_URL}
              target="_blank"
              rel="noopener noreferrer"
              class="text-text-primary underline hover:no-underline"
              >{$t`Learn how to open links in your browser →`}</a
            >
          </p>
        {:else if data.redirectUrl}
          <div class="flex flex-col items-center justify-center gap-4">
            <ProgressRing class="text-fg-primary size-14" />
            <p
              class="text-md text-text-tertiary text-center font-medium text-balance"
            >
              {$t`You are being redirected to the app ${data.redirectUrl} for signin.`}
            </p>
          </div>
        {:else}
          <h1 class="text-text-primary mb-3 text-center text-2xl font-medium">
            {$t`Browser Not Supported`}
          </h1>
          <p
            class="text-md text-text-tertiary text-center font-medium text-balance"
          >
            <Trans
              >Your browser is not supported. Please visit our <a
                href={SUPPORT_URL}
                target="_blank"
                rel="noopener noreferrer"
                class="text-text-primary underline hover:no-underline"
                >support page</a
              > for more information and try again with a different browser.</Trans
            >
          </p>
        {/if}
      </div>
      {#if data.redirectUrl && !data.noRedirect}
        <Button variant="primary" onclick={handleRedirect}>
          {$t`Go to the app`}
          {#if countdown >= 0}
            - (<span class="tabular-nums">{countdown}</span>){/if}
        </Button>
      {/if}
    </div>
  </div>
</div>
