<script lang="ts">
  import type { PageProps } from "./$types";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { onMount } from "svelte";
  import { createRedirectURL } from "$lib/utils/openID";
  import { sessionStore } from "$lib/stores/session.store";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { channelStore } from "$lib/stores/channelStore";

  const { data }: PageProps = $props();

  let timedOut = $state(false);

  onMount(async () => {
    const channel = await channelStore.establish({ pending: true });
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
      "ii-direct-authorize-openid",
      JSON.stringify({
        origin: channel.origin,
        state: next.searchParams.get("state"),
      }),
    );
    window.location.assign(next);
  });
</script>

{#if timedOut}
  <Dialog>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Something went wrong`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base font-medium">
      <Trans>
        It seems like the connection with the service could not be established.
        Try a different browser; if the issue persists, contact support.
      </Trans>
    </p>
    <Button onclick={() => window.close()} variant="secondary">
      <RotateCcwIcon class="size-4" />
      <span>{$t`Return to app`}</span>
    </Button>
  </Dialog>
{/if}
