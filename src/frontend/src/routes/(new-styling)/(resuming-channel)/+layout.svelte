<script lang="ts">
  import type { LayoutProps } from "./$types";
  import AuthorizationChannel from "$lib/components/utils/AuthorizationChannel.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { t } from "$lib/stores/locale.store";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";

  const pendingChannelOrigin = sessionStorage.getItem(
    "ii-pending-channel-origin",
  );

  const { children }: LayoutProps = $props();
</script>

{#if pendingChannelOrigin !== null}
  <AuthorizationChannel options={{ allowedOrigin: pendingChannelOrigin }}>
    {@render children()}
  </AuthorizationChannel>
{:else}
  <Dialog>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Connection closed`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base font-medium">
      {$t`It seems like the connection with the service could not be re-established.`}
    </p>
    <Button onclick={() => window.close()} variant="secondary">
      <RotateCcwIcon class="size-4" />
      <span>{$t`Return to app`}</span>
    </Button>
  </Dialog>
{/if}
