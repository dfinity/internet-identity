<script lang="ts">
  import { establishedChannelStore } from "$lib/stores/channelStore";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import { t } from "$lib/stores/locale.store";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";

  type NewProvider =
    | { type: "passkey" }
    | { type: "openid"; logo: string; name: string }
    | { type: "sso"; name: string };

  interface OpenIdNotConnectedArgs {
    providerName: string;
    providerLogo?: string;
    userName?: string;
    userEmail?: string;
    resume: () => Promise<void>;
    cancel: () => void;
  }

  interface OpenIdAlreadyLinkedArgs {
    providerName: string;
    providerLogo?: string;
    userName?: string;
    userEmail?: string;
    signIn: () => Promise<void>;
    cancel: () => void;
  }

  interface MethodSwitchArgs {
    previous: LastUsedIdentity;
    newProvider: NewProvider;
    proceed: () => Promise<void>;
  }

  interface Props {
    onSignIn: (identityNumber: bigint) => Promise<void>;
    onSignUp: (identityNumber: bigint) => Promise<void>;
    onError: (error: unknown) => void;
    onOpenIdNotConnected?: (args: OpenIdNotConnectedArgs) => void;
    onOpenIdAlreadyLinked?: (args: OpenIdAlreadyLinkedArgs) => void;
    onMethodSwitch?: (args: MethodSwitchArgs) => void;
    onSwitchMode?: () => void;
    mode?: "signin" | "signup" | "both";
  }

  const {
    onSignIn,
    onSignUp,
    onError,
    onOpenIdNotConnected,
    onOpenIdAlreadyLinked,
    onMethodSwitch,
    onSwitchMode,
    mode = "both",
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
  {onOpenIdNotConnected}
  {onOpenIdAlreadyLinked}
  {onMethodSwitch}
  {onSwitchMode}
  {mode}
  withinDialog
>
  <AuthorizeHeader origin={$establishedChannelStore.origin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    {$t`Sign in to Internet Identity`}
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    {$t`to continue to ${dappName}`}
  </p>
</AuthWizard>
