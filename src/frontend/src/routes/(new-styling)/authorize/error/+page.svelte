<script lang="ts">
  import { page } from "$app/state";
  import { afterNavigate, beforeNavigate, goto } from "$app/navigation";
  import type { AfterNavigate, BeforeNavigate } from "@sveltejs/kit";
  import { t } from "$lib/stores/locale.store";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";

  type ErrorCode =
    | "unable-to-connect"
    | "connection-closed"
    | "invalid-request"
    | "unverified-origin"
    | "delegation-failed";

  const messages: Record<ErrorCode, { title: string; description: string }> = {
    "unable-to-connect": {
      title: $t`Unable to connect`,
      description: $t`There was an issue connecting with the application. Try a different browser; if the issue persists, contact the developer.`,
    },
    "connection-closed": {
      title: $t`Connection closed`,
      description: $t`It seems like the connection with the service could not be established. Try a different browser; if the issue persists, contact support.`,
    },
    "invalid-request": {
      title: $t`Invalid request`,
      description: $t`It seems like an invalid authentication request was received.`,
    },
    "unverified-origin": {
      title: $t`Unverified origin`,
      description: $t`It seems like the request could not be processed.`,
    },
    "delegation-failed": {
      title: $t`Authentication failed`,
      description: $t`Something went wrong while creating your delegation. Please try again; if the issue persists, contact support.`,
    },
  };

  const code = page.url.searchParams.get("code");
  const error = $derived(
    code !== null && code in messages
      ? messages[code as ErrorCode]
      : {
          title: $t`Something went wrong`,
          description: $t`An unexpected error occurred. Please try again; if the issue persists, contact support.`,
        },
  );

  const PAGE_RELOADED_KEY = "ii-error-page-reloaded";

  const trackPageReload = (navigation: BeforeNavigate) => {
    if (navigation.type === "leave") {
      sessionStorage.setItem(PAGE_RELOADED_KEY, "true");
    }
  };
  const redirectOnEntryExceptReload = (navigation: AfterNavigate) => {
    const isReload = sessionStorage.getItem(PAGE_RELOADED_KEY) === "true";
    sessionStorage.removeItem(PAGE_RELOADED_KEY);
    if (navigation.type === "enter" && !isReload) {
      goto("/", { replaceState: true });
    }
  };

  beforeNavigate(trackPageReload);
  afterNavigate(redirectOnEntryExceptReload);
</script>

<div class="flex min-h-dvh flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div
    class="flex flex-1 flex-col items-center justify-center max-sm:items-stretch sm:max-w-100 sm:self-center"
  >
    <AuthPanel>
      <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
        <CircleAlertIcon class="size-6" />
      </FeaturedIcon>
      <h1 class="text-text-primary mb-3 text-2xl font-medium">
        {error.title}
      </h1>
      <p class="text-text-tertiary mb-6 text-base font-medium">
        {error.description}
      </p>
      <Button onclick={() => window.close()} variant="secondary">
        <RotateCcwIcon class="size-4" />
        <span>{$t`Return to app`}</span>
      </Button>
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
