<script lang="ts">
  /**
   * Terminal "Email verified" step shared between the recovery-email
   * setup wizard and the verified-email wizard. Reached when the
   * polling loop sees `RegistrationSucceeded`; the user clicks Done
   * and the host runs whatever cleanup it wants (close the dialog,
   * fire a toast, refresh layout data).
   *
   * `flow` switches the body copy between the two callers but
   * everything else — icon, layout, button — is shared verbatim so
   * the two flows feel like one design.
   */
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { CheckIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    /** The address that was just verified — rendered in bold inside
     *  the body copy. */
    address: string;
    /** Switches the body sentence. "recovery" → "<address> is now
     *  your recovery email"; "verified" → "<address> has been added
     *  to your verified emails". */
    flow: "recovery" | "verified";
    /** Triggered when the user dismisses the success card. The host
     *  is expected to close the wizard dialog. */
    onDone: () => void;
  }

  const { address, flow, onDone }: Props = $props();
</script>

<div class="flex flex-1 flex-col items-center text-center">
  <FeaturedIcon variant="success" size="xl" class="mb-5">
    <CheckIcon class="size-7" />
  </FeaturedIcon>
  <h1 class="text-text-primary mb-2 text-2xl font-medium">
    {$t`Email verified`}
  </h1>
  <p class="text-text-secondary mb-8 max-w-xs text-base font-medium">
    {#if flow === "recovery"}
      <Trans>
        <strong class="text-text-primary break-all">{address}</strong> is now your
        recovery email.
      </Trans>
    {:else}
      <Trans>
        <strong class="text-text-primary break-all">{address}</strong> has been added
        to your verified emails.
      </Trans>
    {/if}
  </p>
  <button class="btn btn-primary btn-lg w-full" onclick={onDone}>
    {$t`Done`}
  </button>
</div>
