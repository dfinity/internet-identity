<script lang="ts">
  import { establishedChannelStore } from "$lib/stores/channelStore";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onSignIn: (identityNumber: bigint) => Promise<void>;
    onSignUp: (identityNumber: bigint) => Promise<void>;
    onUpgrade: (identityNumber: bigint) => Promise<void>;
    onError: (error: unknown) => void;
  }

  const { onSignIn, onSignUp, onUpgrade, onError }: Props = $props();

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) => dapp.hasOrigin($establishedChannelStore.origin)),
  );
</script>

<AuthWizard {onSignIn} {onSignUp} {onUpgrade} {onError}>
  <AuthorizeHeader origin={$establishedChannelStore.origin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    {$t`Choose method`}
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    {#if dapp?.name !== undefined}
      {@const application = dapp.name}
      <Trans>
        to continue with <b>{application}</b>
      </Trans>
    {:else}
      <Trans>to continue with this app</Trans>
    {/if}
  </p>
</AuthWizard>
