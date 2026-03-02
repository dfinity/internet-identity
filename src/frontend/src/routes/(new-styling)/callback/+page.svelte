<script lang="ts">
  import { analytics } from "$lib/utils/analytics/analytics";
  import { onMount } from "svelte";
  import { goto } from "$app/navigation";
  import { sendUrlToOpener } from "./utils";

  analytics.event("page-redirect-callback");

  onMount(async () => {
    // If OpenID flow was opened within same window as II,
    // authorize with default account directly instead.
    const openIdAuthorizeState = sessionStorage.getItem(
      "ii-openid-authorize-state",
    );
    if (openIdAuthorizeState !== null) {
      const next = new URL(window.location.href);
      next.pathname = "/resume-openid-authorize";
      await goto(next);
      return;
    }

    // User was returned here after redirect from a OpenID flow callback,
    // these flows are always handled in a popup and the callback url is
    // returned to the opener window through a shared BroadcastChannel.
    sendUrlToOpener();
  });
</script>
