<script lang="ts">
  import type { ChannelError } from "$lib/stores/channelStore";
  import { t } from "$lib/stores/locale.store";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";

  const errorMessages: Record<
    ChannelError,
    { title: string; description: string }
  > = {
    "unable-to-connect": {
      title: $t`Unable to connect`,
      description: $t`There was an issue connecting with the application. Try a different browser; if the issue persists, contact the developer.`,
    },
    "connection-closed": {
      title: $t`Connection closed`,
      description: $t`It seems like the connection with the service could not be established. Try a different browser; if the issue persists, contact support.`,
    },
    "invalid-request": {
      title: $t`Invalid request`,
      description: $t`It seems like an invalid authentication request was received.`,
    },
    "unverified-origin": {
      title: $t`Unverified origin`,
      description: $t`It seems like the request could not be processed.`,
    },
    "unsupported-browser": {
      title: $t`Unsupported browser`,
      description: $t`This browser is not supported.`,
    },
    "delegation-failed": {
      title: $t`Authentication failed`,
      description: $t`Something went wrong while creating your delegation. Please try again; if the issue persists, contact support.`,
    },
    "attributes-failed": {
      title: $t`Attribute sharing failed`,
      description: $t`Something went wrong while sharing attributes. Please try again; if the issue persists, contact support.`,
    },
  };

  interface Props {
    error: ChannelError;
  }

  const { error }: Props = $props();
  const messages = $derived(errorMessages[error]);
</script>

<div class="flex min-h-dvh flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div
    class="flex flex-1 flex-col items-center justify-center max-sm:items-stretch sm:max-w-100 sm:self-center"
  >
    <AuthPanel>
      <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
        <CircleAlertIcon class="size-6" />
      </FeaturedIcon>
      <h1 class="text-text-primary mb-3 text-2xl font-medium">
        {messages.title}
      </h1>
      <p class="text-text-tertiary mb-6 text-base font-medium">
        {messages.description}
      </p>
      <Button onclick={() => window.close()} variant="secondary">
        <RotateCcwIcon class="size-4" />
        <span>{$t`Return to app`}</span>
      </Button>
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
