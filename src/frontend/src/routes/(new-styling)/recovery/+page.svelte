<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import CancelRecovery from "./components/CancelRecovery.svelte";
  import Trans from "../../../lib/components/locale/Trans.svelte";
  import { t } from "$lib/stores/locale.store";
  import { goto } from "$app/navigation";
  import { RefreshCcw } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";

  let showCancelDialog = $state(false);
</script>

<div class="mt-auto flex flex-col sm:my-auto">
  <FeaturedIcon size="lg" class="mb-4">
    <RefreshCcw class="size-5" />
  </FeaturedIcon>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">
    {$t`Recover your identity`}
  </h1>
  <p class="text-text-tertiary mb-2 text-sm">
    <Trans>
      Ensure you are in a private place before entering your recovery phrase. It
      grants full access to your identity and must remain confidential.
    </Trans>
  </p>
  <p class="text-text-tertiary mb-6 text-sm">
    <Trans>
      Have your phrase ready so the next step is quick and accurate. If you need
      a moment to prepare, feel free to pause here before moving forward.
    </Trans>
  </p>
  <Button href="/recovery/enter" size="xl" class="mb-3">
    {$t`Get started`}
  </Button>
  <Button
    onclick={() => (showCancelDialog = true)}
    variant="secondary"
    size="xl"
  >
    {$t`Cancel`}
  </Button>
</div>

{#if showCancelDialog}
  <Dialog onClose={() => (showCancelDialog = false)}>
    <CancelRecovery
      onCancel={() => goto("/login")}
      onClose={() => (showCancelDialog = false)}
    />
  </Dialog>
{/if}
