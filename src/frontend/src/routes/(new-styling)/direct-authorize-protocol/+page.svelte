<script lang="ts">
  import type { PageProps } from "./$types";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { onMount } from "svelte";
  import { createRedirectURL } from "$lib/utils/openID";
  import { HeartbeatServer } from "@slide-computer/signer-web";
  import { sessionStore } from "$lib/stores/session.store";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";

  // The maximum amount of time in milliseconds we're willing to wait before
  // having to consider the establishment of the ICRC-29 channel a failure.
  //
  // Waiting any longer than this would likely not result in a successful ICRC-29
  // channel establishment while making the user wait too long for visual feedback.
  const establishTimeout = 2000;

  const { data }: PageProps = $props();

  let timedOut = $state(false);

  onMount(() => {
    new HeartbeatServer({
      status: "pending",
      onEstablish(origin: string): void {
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
          JSON.stringify({ origin, state: next.searchParams.get("state") }),
        );
        window.location.assign(next);
      },
      establishTimeout,
      onEstablishTimeout(): void {
        timedOut = true;
      },
      onDisconnect(): void {
        // Unreachable, we redirect immediately after establishing
      },
    });
  });
</script>

{#if timedOut}
  <Dialog>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      Something went wrong
    </h1>
    <p class="text-md text-text-tertiary mb-6 font-medium">
      It seems like the connection with the service could not be established.
      Try a different browser; if the issue persists, contact support.
    </p>
    <Button onclick={() => window.close()} variant="secondary">
      <RotateCcwIcon size="1rem" />
      Return to app
    </Button>
  </Dialog>
{/if}
