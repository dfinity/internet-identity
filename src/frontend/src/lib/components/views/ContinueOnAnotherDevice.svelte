<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { CopyIcon } from "@lucide/svelte";
  import QrCode from "$lib/components/ui/QrCode.svelte";

  interface Props {
    url: URL;
  }

  const { url }: Props = $props();

  const handleCopyLink = () => navigator.clipboard.writeText(url.href);
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
  <Button onclick={handleCopyLink} variant="secondary" size="lg" class="mt-6">
    <span>
      {url.host + url.pathname}
    </span>
    <CopyIcon size="1.25rem" />
  </Button>
</div>
