<script lang="ts">
  import { TriangleAlertIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    onConfirm: () => void;
    onClose: () => void;
  }

  const { onConfirm, onClose }: Props = $props();

  let acknowledged = $state(false);
</script>

<Dialog {onClose} width="wider">
  <div class="flex flex-col gap-5 p-1">
    <FeaturedIcon size="lg" variant="warning" class="self-start">
      <TriangleAlertIcon class="size-6" />
    </FeaturedIcon>

    <h2 class="text-text-primary text-2xl font-medium">
      {$t`Are you sure?`}
    </h2>

    <p class="text-text-tertiary text-base text-pretty">
      <Trans>
        Enabling this lets AI assistants on this device ask to sign you in to
        apps using your identity.
      </Trans>
    </p>

    <ul class="text-text-tertiary flex list-disc flex-col gap-2 ps-5 text-sm">
      <li>
        <Trans>It can send messages or move funds on your behalf.</Trans>
      </li>
      <li>
        <Trans>Like any AI, it can hallucinate and make mistakes.</Trans>
      </li>
    </ul>

    <Checkbox
      bind:checked={acknowledged}
      label={$t`I understand the risks.`}
      labelClass="text-sm"
    />

    <button
      class="btn btn-primary btn-lg w-full"
      onclick={onConfirm}
      disabled={!acknowledged}
    >
      {$t`Enable MCP access`}
    </button>
  </div>
</Dialog>
