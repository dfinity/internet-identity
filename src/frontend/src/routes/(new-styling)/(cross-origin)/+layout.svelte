<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { getPrimaryOrigin, IFRAME_PARENT_PARAM } from "$lib/globals";
  import { forwardMessage, isForwardedMessage } from "./utils";

  const { children, data }: LayoutProps = $props();

  const primaryOrigin = getPrimaryOrigin();
  const isDesktopSafari =
    /^((?!chrome|chromium|android|edg|opr|firefox).)*safari/i.test(
      navigator.userAgent,
    ) && !/mobile/i.test(navigator.userAgent);
  const isIOS = /iPad|iPhone|iPod/.test(navigator.userAgent);
  const supportsIframeCredentialsCreate = !isIOS && !isDesktopSafari;
  const shouldEmbed =
    supportsIframeCredentialsCreate &&
    data.legacyProtocol &&
    primaryOrigin !== undefined &&
    window.location.origin !== primaryOrigin;
  const iframeSrc = $derived.by(() => {
    if (primaryOrigin === undefined) {
      return;
    }
    const url = new URL(window.location.href);
    url.hostname = new URL(primaryOrigin).hostname;
    url.searchParams.set("feature_flag_guided_upgrade", "true");
    url.searchParams.set(IFRAME_PARENT_PARAM, window.location.origin);
    return url.href;
  });

  let iframeRef = $state<HTMLIFrameElement>();

  $effect(() => {
    if (!shouldEmbed) {
      return;
    }
    const listener = (event: MessageEvent) => {
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
      } else if (event.source === window.opener) {
        // Wrap and forward other messages towards iframe
        (iframeRef.contentWindow as Window).postMessage(
          forwardMessage(event.data, event.origin),
          primaryOrigin,
        );
      }
    };
    window.addEventListener("message", listener);
    return () => {
      window.removeEventListener("message", listener);
    };
  });
</script>

{#if shouldEmbed && iframeSrc !== undefined}
  <iframe
    bind:this={iframeRef}
    src={iframeSrc}
    allow="publickey-credentials-create; publickey-credentials-get"
    title="Embedded page"
    class="absolute inset-0 box-border h-full w-full border-t border-t-orange-500"
  ></iframe>
{:else}
  {@render children()}
{/if}
