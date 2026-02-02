<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import MigrationSuccessIllustration from "$lib/components/illustrations/MigrationSuccessIllustration.svelte";
  import { onMount } from "svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { establishedChannelStore } from "$lib/stores/channelStore";
  import { DelegationResultSchema } from "$lib/utils/transport/utils";

  const COUNTDOWN_SECONDS = 5;
  let countdown = $state(COUNTDOWN_SECONDS);
  let intervalId: number | undefined;
  let redirected = $state(false);

  const handleRedirect = async (): Promise<void> => {
    window.clearInterval(intervalId);
    if (redirected) {
      return;
    }
    redirected = true;
    const { requestId, delegationChain } =
      await authorizationStore.authorize(undefined);
    const result = DelegationResultSchema.encode(delegationChain);
    await $establishedChannelStore.send({
      jsonrpc: "2.0",
      id: requestId,
      result,
    });
  };

  onMount(() => {
    intervalId = window.setInterval(() => {
      countdown -= 1;
      if (countdown <= 0) {
        handleRedirect();
      }
    }, 1000);

    return () => {
      window.clearInterval(intervalId);
    };
  });
</script>

<div
  class="flex flex-1 flex-col items-stretch justify-end p-4 sm:max-w-100 sm:items-center sm:justify-center"
>
  <div
    class="text-text-primary flex h-50 flex-1 items-center justify-center sm:flex-none"
  >
    <MigrationSuccessIllustration />
  </div>
  <div class="mb-8 flex flex-col gap-2">
    <h1 class="text-text-primary mb-3 text-center text-2xl font-medium">
      {$t`Identity upgraded!`}
    </h1>
    <p
      class="text-text-tertiary text-center text-base font-medium text-balance"
    >
      <Trans>
        You no longer need to remember your identity number and can use your
        fingerprint, face or screen lock instead.
      </Trans>
    </p>
  </div>
  <Button variant="primary" onclick={handleRedirect}>
    {#if countdown >= 0}
      <Trans>
        Go to the app - (<span class="tabular-nums">{countdown}</span>)
      </Trans>
    {:else}
      <Trans>Go to the app</Trans>
    {/if}
  </Button>
</div>
