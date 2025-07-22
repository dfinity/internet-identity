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

<div class="flex flex-col">
  <QrCode text={url.href} class="my-6 size-32 self-center" />
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    Continue on another device
  </h1>
  <p class="text-md text-text-tertiary font-medium text-balance sm:text-center">
    Scan the above QR code with your
    <b class="text-text-primary">new device</b> or enter the URL manually.
  </p>
  <Tooltip
    label="Link copied to clipboard"
    hidden={!copied}
    arrow={false}
    manual
  >
    <Button
      onclick={handleCopyLink}
      variant="secondary"
      size="xl"
      class="mt-6"
      aria-label="Pairing link"
    >
      <span>
        {url.host + url.pathname + url.hash}
      </span>
      <CopyIcon size="1.25rem" />
    </Button>
  </Tooltip>
</div>
