<script lang="ts">
  import { onMount } from "svelte";
  import AppleConnectingView from "$lib/components/views/AppleConnectingView.svelte";
  import { callWhenAppleSheetIsCanceled } from "$lib/utils/appleSheet";
  import { APPLE_ORIGIN } from "$lib/utils/openID";

  /** The Apple authorize URL the opener put in the fragment, if it is one. */
  const readAppleAuthorizeUrl = (): URL | undefined => {
    try {
      const url = new URL(decodeURIComponent(window.location.hash.slice(1)));
      return url.origin === APPLE_ORIGIN ? url : undefined;
    } catch {
      return undefined;
    }
  };

  onMount(() => {
    const appleAuthorizeUrl = readAppleAuthorizeUrl();
    if (appleAuthorizeUrl === undefined) {
      window.close();
      return;
    }
    callWhenAppleSheetIsCanceled(() => window.close());
    window.location.replace(appleAuthorizeUrl);
  });
</script>

<!-- The Sign in with Apple popup's first page: it sends itself to Apple, whose
     native sheet then shows over it. -->
<div class="flex min-h-dvh items-center justify-center">
  <AppleConnectingView onReturnToApp={() => window.close()} />
</div>
