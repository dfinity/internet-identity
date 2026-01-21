<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { getPrimaryOrigin } from "$lib/globals";
  import { forwardMessage, isForwardedMessage } from "./utils";

  const { children, data }: LayoutProps = $props();

  const primaryOrigin = getPrimaryOrigin();
  const shouldEmbed =
    data.legacyProtocol &&
    primaryOrigin !== undefined &&
    window.location.origin !== primaryOrigin;
  const iframeSrc = $derived.by(() => {
    const url = new URL(window.location.href);
    url.origin = primaryOrigin;
    url.searchParams.set("feature_flag_guided_upgrade", "true");
    return url.href;
  });

  let iframeRef = $state<HTMLIFrameElement>();

  $effect(() => {
    if (!shouldEmbed) {
      return;
    }
    window.addEventListener("message", (event) => {
      if (iframeRef === undefined) {
        return;
      }
      if (
        event.source === iframeRef?.contentWindow &&
        event.origin === primaryOrigin &&
        isForwardedMessage(event)
      ) {
        // Unwrap and forward messages coming from iframe
        window.opener.postMessage(
          event.data.__ii_forwarded.data,
          event.data.__ii_forwarded.origin,
        );
      } else {
        // Wrap and forward other messages towards iframe
        (iframeRef.contentWindow as Window).postMessage(
          forwardMessage(event.data, event.origin),
          primaryOrigin,
        );
      }
    });
  });
</script>

{#if shouldEmbed}
  <iframe
    bind:this={iframeRef}
    src={iframeSrc}
    allow="publickey-credentials-create; publickey-credentials-get"
    title="Embedded page"
    class="absolute inset-0 box-border h-full w-full"
  ></iframe>
{:else}
  {@render children()}
{/if}
