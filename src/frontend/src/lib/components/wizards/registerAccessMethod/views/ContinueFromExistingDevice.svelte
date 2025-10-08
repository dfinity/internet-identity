<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { CopyIcon, CheckIcon } from "@lucide/svelte";
  import QrCode from "$lib/components/ui/QrCode.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { waitFor } from "$lib/utils/utils";

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
      Can't find your identity or passkey?
    </h1>
    <p
      class="text-md text-text-tertiary font-medium text-balance sm:text-center"
    >
      Use a device that already holds your identity to scan or open the URL.
      This will link your identity to the desired device.
    </p>
  </div>
  <QrCode text={url.href} class="size-32 self-center" />
  <Tooltip label="Link copied to clipboard" hidden={!copied} manual>
    <Button
      onclick={handleCopyLink}
      variant="secondary"
      size="xl"
      aria-label="Pairing link"
    >
      <span>
        {url.host + url.pathname + url.hash}
      </span>
      <CopyIcon size="1.25rem" />
    </Button>
  </Tooltip>
</div>
