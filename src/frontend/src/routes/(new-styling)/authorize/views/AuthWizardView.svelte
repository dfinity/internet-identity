<script lang="ts">
  import { establishedChannelStore } from "$lib/stores/channelStore";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import { t } from "$lib/stores/locale.store";
  import type { AuthMode } from "$lib/flows/authFlow.svelte";

  interface Props {
    onSignIn: (identityNumber: bigint) => Promise<void>;
    onSignUp: (identityNumber: bigint) => Promise<void>;
    onError: (error: unknown) => void;
    mode?: AuthMode;
  }

  let {
    onSignIn,
    onSignUp,
    onError,
    mode = $bindable("both"),
  }: Props = $props();

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) => dapp.hasOrigin($establishedChannelStore.origin)),
  );
  const dappName = $derived(
    dapp?.name ?? new URL($establishedChannelStore.origin).hostname,
  );
</script>

<AuthWizard
  {onSignIn}
  {onSignUp}
  {onError}
  bind:mode
  ssoOrigin={$authorizationStore?.effectiveOrigin}
>
  <AuthorizeHeader origin={$establishedChannelStore.origin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    {mode === "signup"
      ? $t`Create an Identity`
      : $t`Sign in to Internet Identity`}
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    {$t`to continue to ${dappName}`}
  </p>
</AuthWizard>
