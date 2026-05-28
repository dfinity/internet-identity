<script lang="ts">
  import { TriangleAlertIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import VerifyPanel from "./VerifyPanel.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    onConfirm: () => void;
    onClose: () => void;
  }

  const { onConfirm, onClose }: Props = $props();

  let acknowledged = $state(false);
</script>

<Dialog {onClose}>
  <div class="flex flex-col gap-5 p-1">
    <FeaturedIcon size="lg" variant="warning" class="self-start">
      <TriangleAlertIcon class="size-6" />
    </FeaturedIcon>

    <h2 class="text-text-primary text-2xl font-medium">
      {$t`Are you sure?`}
    </h2>

    <p class="text-text-tertiary text-base text-pretty">
      <Trans>
        Once enabled, any command-line tool on this device can use your identity
        to sign in to apps.
      </Trans>
    </p>

    <VerifyPanel installCommand="npm install -g @icp-sdk/icp-cli">
      <Trans>
        Only enable this for the official <b>ICP CLI</b>. If you're unsure,
        reinstall it from the
        <a
          href="https://github.com/dfinity/icp-cli"
          target="_blank"
          rel="noopener noreferrer"
          class="text-text-primary font-semibold hover:underline focus-visible:underline"
        >
          official source
        </a>.
      </Trans>
    </VerifyPanel>

    <Checkbox
      bind:checked={acknowledged}
      label={$t`I'm using the official ICP CLI and I trust this device.`}
    />

    <button
      class="btn btn-primary btn-lg w-full"
      onclick={onConfirm}
      disabled={!acknowledged}
    >
      {$t`Enable CLI access`}
    </button>
  </div>
</Dialog>
