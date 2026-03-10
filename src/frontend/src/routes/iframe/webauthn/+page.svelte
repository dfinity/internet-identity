<script lang="ts">
  import { handleCredentialRequest } from "./utils";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig } from "$lib/globals";
  import { waitForWindowReadyRequest } from "$lib/utils/internalPostMessage";
  import { onMount } from "svelte";

  onMount(async () => {
    analytics.event("page-webauthn-iframe");

    // Establish cross-origin connection with parent window
    const targetOrigin = await waitForWindowReadyRequest(
      window.parent,
      // We only establish a connection for the related origins in the II config,
      // incoming requests from other origins are not listed here and ignored.
      //
      // Additionally, the CSP configuration will block any attempt to render II
      // inside an iframe from domains that are not related origins.
      canisterConfig.related_origins[0] ?? [],
    );

    // Get credential and send to parent window
    handleCredentialRequest(window.parent, targetOrigin);
  });
</script>
