<script lang="ts">
  import type { PageProps } from "./$types";
  import { onMount } from "svelte";
  import { createRedirectURL } from "$lib/utils/openID";
  import { sessionStore } from "$lib/stores/session.store";
  import { establishedChannelStore } from "$lib/stores/channelStore";

  const { data }: PageProps = $props();

  onMount(() => {
    const next = createRedirectURL(
      {
        clientId: data.config.client_id,
        authURL: data.config.auth_uri,
        authScope: data.config.auth_scope.join(" "),
      },
      {
        nonce: $sessionStore.nonce,
        mediation: "required",
      },
    );
    sessionStorage.setItem(
      "ii-pending-channel-origin",
      $establishedChannelStore.origin,
    );
    sessionStorage.setItem(
      "ii-openid-authorize-state",
      next.searchParams.get("state")!,
    );
    window.location.assign(next);
  });
</script>
