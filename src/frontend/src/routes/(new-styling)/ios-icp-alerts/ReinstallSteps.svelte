<script lang="ts">
  // A Home Screen entry carries the URL it was added with and the storage partition it was added into, so removing it and adding it again is the only way either changes.
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import { NOTIFICATION_APP_NAME } from "./appName";

  /** Where the user signs in and turns notifications on, which is where the steps for adding the app are handed out. */
  const { host }: { host: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Remove this app`}>
    {#snippet instruction()}
      <Trans>
        Touch and hold the {NOTIFICATION_APP_NAME} icon on your Home Screen, then
        tap Remove App.
      </Trans>
    {/snippet}
  </Step>

  <Step n={2} title={$t`Set up notifications`}>
    {#snippet instruction()}
      <Trans>
        Open {host}, sign in, and turn notifications on. It will show you how to
        add the app back.
      </Trans>
    {/snippet}
  </Step>
</Steps>
