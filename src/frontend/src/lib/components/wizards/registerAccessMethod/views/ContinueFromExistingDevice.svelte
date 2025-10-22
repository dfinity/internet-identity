<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { CopyIcon, CheckIcon } from "@lucide/svelte";
  import QrCode from "$lib/components/ui/QrCode.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { waitFor } from "$lib/utils/utils";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    url: URL;
  }

  const { url }: Props = $props();

  let copied = $state(false);

  const handleCopyLink = async () => {
    await navigator.clipboard.writeText(url.href);
    copied = true;
    await waitFor(700);
    copied = false;
  };
</script>

<div class="mt-4 flex flex-col gap-8">
  <div>
    <h1 class="text-text-primary mb-3 text-xl font-medium sm:text-center">
      {$t`Can't find your identity?`}
    </h1>
    <p
      class="text-md text-text-tertiary font-medium text-balance sm:text-center"
    >
      <Trans>
        Use a device that already holds your identity to scan or open the URL.
        This will link your identity to this device.
      </Trans>
    </p>
  </div>
  <QrCode text={url.href} class="size-32 self-center" />
  <Tooltip label={$t`Link copied to clipboard`} hidden={!copied} manual>
    <Button
      onclick={handleCopyLink}
      variant="secondary"
      size="xl"
      aria-label={$t`Pairing link`}
    >
      <span>
        {url.host + url.pathname + url.hash}
      </span>
      <CopyIcon class="size-5" />
    </Button>
  </Tooltip>
</div>
