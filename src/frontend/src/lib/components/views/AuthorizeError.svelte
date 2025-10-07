<script lang="ts">
  import type { AuthorizationStatus } from "$lib/stores/authorization.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { RotateCcwIcon, CircleAlertIcon } from "@lucide/svelte";

  interface Props {
    status: AuthorizationStatus;
  }

  const { status }: Props = $props();
</script>

{#if status === "orphan" || status === "closed" || status === "invalid" || status === "failure" || status === "unverified-origin"}
  {@const title = {
    orphan: "Missing request",
    closed: "Connection closed",
    invalid: "Invalid request",
    failure: "Something went wrong",
    "unverified-origin": "Unverified origin",
  }[status]}
  {@const description = {
    orphan:
      "There was an issue connecting with the application. Try a different browser; if the issue persists, contact the developer.",
    closed:
      "It seems like the connection with the service could not be established. Try a different browser; if the issue persists, contact support.",
    invalid: "It seems like an invalid authentication request was received.",
    failure:
      "Something went wrong during authentication. Authenticating service was notified and you may close this page.",
    "unverified-origin":
      "There was an error verifying the origin of the request. Authenticating service was notified and you may close this page.",
  }[status]}
  <Dialog>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon size="1.5rem" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">{title}</h1>
    <p class="text-md text-text-tertiary mb-6 font-medium">{description}</p>
    <Button onclick={() => window.close()} variant="secondary">
      <RotateCcwIcon size="1rem" />
      Return to app
    </Button>
  </Dialog>
{:else if status === "late-success"}
  <Dialog>
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <CircleAlertIcon size="1.5rem" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      Authentication successful
    </h1>
    <p class="text-md text-text-tertiary mb-6 font-medium">
      You may close this page.
    </p>
    <Button onclick={() => window.close()} variant="secondary">
      <RotateCcwIcon size="1rem" />
      Return to app
    </Button>
  </Dialog>
{/if}
