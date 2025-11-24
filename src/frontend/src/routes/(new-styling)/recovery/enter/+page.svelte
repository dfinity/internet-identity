<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import CancelRecovery from "../components/CancelRecovery.svelte";
  import Trans from "$lib/components/locale/Trans.svelte";
  import { t } from "$lib/stores/locale.store";
  import { goto } from "$app/navigation";

  let showCancelDialog = $state(false);

  const handleGetStarted = async () => {
    await goto("/recovery");
  };

  const handleCancel = async () => {
    showCancelDialog = false;
    await goto("/login");
  };
</script>

<div
  class="flex flex-1 flex-row items-end justify-center sm:max-w-120 sm:items-center"
>
  <AuthPanel>
    <div class="flex flex-col gap-6">
      <div class="flex flex-col gap-3">
        <h1 class="text-text-primary text-2xl font-medium">
          {$t`Recover your identity`}
        </h1>
        <p class="text-text-tertiary text-sm">
          <Trans>
            Have your recovery phrase ready before continuing. Keep it private.
            Don’t let anyone watch you enter it. You can reset it later in your
            dashboard if you want a new one.
          </Trans>
        </p>
      </div>
      <div class="flex flex-col gap-3">
        <Button size="xl" variant="primary" onclick={handleGetStarted}>
          {$t`Get started`}
        </Button>
        <Button
          size="xl"
          variant="secondary"
          onclick={() => (showCancelDialog = true)}
        >
          {$t`Cancel`}
        </Button>
      </div>
    </div>
  </AuthPanel>
</div>

{#if showCancelDialog}
  <Dialog onClose={() => (showCancelDialog = false)}>
    <CancelRecovery
      onCancel={handleCancel}
      onClose={() => (showCancelDialog = false)}
    />
  </Dialog>
{/if}
