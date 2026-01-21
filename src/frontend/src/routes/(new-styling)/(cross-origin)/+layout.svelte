<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { getPrimaryOrigin } from "$lib/globals";
  import { building } from "$app/environment";
  import { ForwardedMessage, isForwardedMessage } from "./utils";

  const { children, data }: LayoutProps = $props();

  const primaryOrigin = getPrimaryOrigin();
  const shouldEmbed =
    data.legacyProtocol &&
    primaryOrigin !== undefined &&
    window.location.origin !== primaryOrigin;

  let iframeRef = $state<HTMLIFrameElement>();

  $effect(() => {
    if (!shouldEmbed) {
      return;
    }
    window.addEventListener("message", (event) => {
      if (
        event.source === iframeRef.contentWindow &&
        event.origin === primaryOrigin &&
        isForwardedMessage(event)
      ) {
        console.log("unwrap message", event);
        // Unwrap and forward messages coming from iframe
        window.opener.postMessage(
          event.data.__ii_forwarded.data,
          event.data.__ii_forwarded.origin,
        );
      } else {
        console.log("wrap message", event);
        // Wrap and forward other messages towards iframe
        const message: ForwardedMessage = {
          __ii_forwarded: {
            data: event.data,
            origin: event.origin,
          },
        };
        (iframeRef.contentWindow as Window).postMessage(message, primaryOrigin);
      }
    });
  });
</script>

{#if shouldEmbed}
  <iframe
    bind:this={iframeRef}
    src={primaryOrigin +
      window.location.pathname +
      window.location.search +
      window.location.hash}
    allow="publickey-credentials-create; publickey-credentials-get"
    class="absolute inset-0 box-border h-full w-full"
  ></iframe>
{:else}
  {@render children()}
{/if}
