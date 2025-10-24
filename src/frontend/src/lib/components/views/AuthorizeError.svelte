<script lang="ts">
  import type { AuthorizationStatus } from "$lib/stores/authorization.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { RotateCcwIcon, CircleAlertIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    status: AuthorizationStatus;
  }

  const { status }: Props = $props();
</script>

{#if status === "orphan" || status === "closed" || status === "invalid" || status === "failure" || status === "unverified-origin"}
  {@const title = {
    orphan: $t`Missing request`,
    closed: $t`Connection closed`,
    invalid: $t`Invalid request`,
    failure: $t`Something went wrong`,
    "unverified-origin": $t`Unverified origin`,
  }[status]}
  {@const description = {
    orphan: $t`There was an issue connecting with the application. Try a different browser; if the issue persists, contact the developer.`,
    closed: $t`It seems like the connection with the service could not be established. Try a different browser; if the issue persists, contact support.`,
    invalid: $t`It seems like an invalid authentication request was received.`,
    failure: $t`Something went wrong during authentication. Authenticating service was notified and you may close this page.`,
    "unverified-origin": $t`There was an error verifying the origin of the request. Authenticating service was notified and you may close this page.`,
  }[status]}
  <Dialog>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">{title}</h1>
    <p class="text-text-tertiary mb-6 text-base font-medium">{description}</p>
    <Button onclick={() => window.close()} variant="secondary">
      <RotateCcwIcon class="size-4" />
      <span>{$t`Return to app`}</span>
    </Button>
  </Dialog>
{:else if status === "late-success"}
  <Dialog>
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Authentication successful`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base font-medium">
      {$t`You may close this page.`}
    </p>
    <Button onclick={() => window.close()} variant="secondary">
      <RotateCcwIcon class="size-4" />
      <span>{$t`Return to app`}</span>
    </Button>
  </Dialog>
{/if}
