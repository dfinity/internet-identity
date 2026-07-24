<script lang="ts">
  import { ExternalLinkIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { t } from "$lib/stores/locale.store";
  import type { ManageHandoffFlow } from "$lib/flows/manageHandoffFlow.svelte";

  interface Props {
    /** The flow whose overlay/confirmation state this renders. */
    flow: ManageHandoffFlow;
    /** Body copy for the confirmation step. */
    description?: string;
    /** Label for the confirm button. */
    buttonLabel?: string;
  }

  const { flow, description, buttonLabel }: Props = $props();
</script>

<!-- Backdrop while an OpenID/SSO provider overlay is showing during the ceremony. -->
{#if flow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}

<!--
  After authenticating, Safari/strict Firefox have spent the click's transient
  activation, so we can't open the new tab automatically. This confirmation
  step's own button click supplies fresh activation for `window.open`.
-->
{#if flow.pending !== undefined}
  <Dialog onClose={flow.dismiss}>
    <div class="flex flex-col">
      <h2 class="text-text-primary my-2 self-start text-2xl font-medium">
        {$t`You're signed in`}
      </h2>
      <p class="text-text-secondary mb-6 self-start text-sm">
        {description ??
          $t`Open Internet Identity in a new tab to manage your access methods and recovery options.`}
      </p>
      <button
        class="btn btn-primary btn-lg w-full gap-2"
        onclick={flow.confirm}
      >
        <ExternalLinkIcon class="size-4" aria-hidden="true" />
        {buttonLabel ?? $t`Open manage`}
      </button>
    </div>
  </Dialog>
{/if}
