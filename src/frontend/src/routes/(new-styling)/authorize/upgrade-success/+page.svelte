<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import MigrationSuccessIllustration from "$lib/components/illustrations/MigrationSuccessIllustration.svelte";
  import { onMount, onDestroy } from "svelte";

  let countdown = 5;
  let intervalId: ReturnType<typeof setInterval> | undefined;
  let redirected = false;

  const handleRedirect = () => {
    if (redirected) return;
    redirected = true;
    if (intervalId) clearInterval(intervalId);
    authorizationStore.authorize(undefined);
  };

  onMount(() => {
    intervalId = setInterval(() => {
      if (redirected) {
        if (intervalId) clearInterval(intervalId);
        return;
      }
      countdown -= 1;
      if (countdown <= 0) {
        handleRedirect();
      }
    }, 1000);
  });

  onDestroy(() => {
    if (intervalId) clearInterval(intervalId);
  });
</script>

<div class="flex flex-1 flex-col items-center justify-center p-4 sm:max-w-100">
  <div class="text-text-primary flex h-50 items-center justify-center">
    <MigrationSuccessIllustration />
  </div>
  <div class="mb-8 flex flex-col gap-2">
    <h1 class="text-text-primary mb-3 text-center text-2xl font-medium">
      Identity upgraded!
    </h1>
    <p class="text-md text-text-tertiary text-center font-medium text-balance">
      You no longer need to remember your identity number and can use your
      fingerprint, face or screen lock instead.
    </p>
  </div>
  <Button variant="primary" onclick={handleRedirect}>
    Go to the app {#if countdown > 0}
      - ({countdown}){/if}
  </Button>
</div>
