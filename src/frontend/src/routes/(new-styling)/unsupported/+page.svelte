<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import ShieldIllustration from "$lib/components/illustrations/ShieldIllustration.svelte";
  import { SUPPORT_URL } from "$lib/config";
  import { onMount } from "svelte";
  import type { PageData } from "../$types";

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
    if (data.redirectUrl) {
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

<div
  class="flex flex-1 flex-col items-stretch justify-end p-4 sm:max-w-100 sm:items-center sm:justify-center"
>
  <div
    class="text-text-primary flex h-50 flex-1 items-center justify-center sm:flex-none"
  >
    <ShieldIllustration />
  </div>
  <div class="mb-8 flex flex-col gap-2">
    {#if data.redirectUrl}
      <h1 class="text-text-primary mb-3 text-center text-2xl font-medium">
        Browser Not Supported
      </h1>
      <p
        class="text-md text-text-tertiary text-center font-medium text-balance"
      >
        Redirecting you to the application <span class="text-text-primary"
          >{data.redirectUrl}</span
        >
      </p>
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
          class="text-text-primary underline hover:no-underline">support page</a
        > for more information and try again with a different browser.
      </p>
    {/if}
  </div>
  {#if data.redirectUrl}
    <Button variant="primary" onclick={handleRedirect}>
      Go to the app {#if countdown >= 0}
        - (<span class="tabular-nums">{countdown}</span>){/if}
    </Button>
  {/if}
</div>
