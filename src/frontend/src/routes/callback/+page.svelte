<script lang="ts">
  import { analytics } from "$lib/utils/analytics/analytics";
  import { onMount } from "svelte";
  import { nonNullish } from "@dfinity/utils";
  import { goto } from "$app/navigation";

  analytics.event("page-redirect-callback");

  onMount(() => {
    // If OpenID flow was opened within same window as II,
    // redirect with JWT to authorize page directly instead.
    const redirectState = sessionStorage.getItem("ii-oauth-redirect-state");
    const searchParams = new URLSearchParams(window.location.hash.slice(1));
    const state = searchParams.get("state");
    const jwt = searchParams.get("id_token");
    if (
      nonNullish(redirectState) &&
      redirectState === state &&
      nonNullish(jwt)
    ) {
      goto("/authorize/openid?rpc", { state: { jwt } });
      return;
    }

    // User was returned here after redirect from a OpenID flow callback,
    // these flows are always handled in a popup and the callback url is
    // returned to the opener window through the PostMessage API.
    window.opener.postMessage(window.location.href, window.location.origin);
  });
</script>
