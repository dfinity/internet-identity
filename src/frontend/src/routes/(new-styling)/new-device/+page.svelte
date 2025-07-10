<script lang="ts">
  import Header from "$lib/components/layout/Header.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { RegisterAccessMethodFlow } from "$lib/flows/registerAccessMethodFlow.svelte.js";
  import type { PageProps } from "./$types";
  import { CopyIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { isNullish } from "@dfinity/utils";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import { goto } from "$app/navigation";

  const { data }: PageProps = $props();

  const registerAccessMethodFlow = new RegisterAccessMethodFlow(
    data.identityNumber,
  );

  const handleCopyCode = () => {
    if (isNullish(registerAccessMethodFlow.confirmationCode)) {
      return;
    }
    navigator.clipboard.writeText(registerAccessMethodFlow.confirmationCode);
  };
  const handleCreatePasskey = async () => {
    await registerAccessMethodFlow.createPasskey();
    await goto("/");
  };
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <AuthPanel class="sm:max-w-100">
      <div class="flex-1"></div>
      {#if registerAccessMethodFlow.view === "confirmSignIn"}
        <PasskeyIllustration class="text-text-primary mt-4 mb-8 h-32" />
        <h1 class="text-text-primary mb-3 text-2xl font-medium">
          Confirm your sign-in
        </h1>
        <p class="text-md text-text-tertiary mb-8 font-medium text-balance">
          You're signing in as
          <b class="text-text-primary">
            {registerAccessMethodFlow.identityName ?? data.identityNumber}
          </b>.
          <br /><br />
          To continue, create a passkey to secure your identity and simplify future
          sign-ins.
        </p>
        <Button onclick={handleCreatePasskey} size="xl">Create passkey</Button>
      {:else}
        <ConfirmDeviceIllustration class="text-text-primary mt-4 mb-8 h-32" />
        <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
          Confirm this device
        </h1>
        <p
          class="text-md text-text-tertiary mb-8 font-medium text-balance sm:text-center"
        >
          To confirm it's you, enter below code on your
          <b class="text-text-primary">existing device</b>.
        </p>
        {#if registerAccessMethodFlow.view === "confirmDevice"}
          <Button
            onclick={handleCopyCode}
            variant="secondary"
            size="lg"
            disabled={isNullish(registerAccessMethodFlow.confirmationCode)}
          >
            {#if registerAccessMethodFlow.confirmationCode}
              <span>
                {registerAccessMethodFlow.confirmationCode}
              </span>
              <CopyIcon size="1.25rem" />
            {:else}
              <ProgressRing />
              <span>Generating code...</span>
            {/if}
          </Button>
        {:else if registerAccessMethodFlow.view === "linkExpired"}
          <Alert
            variant="error"
            title="This link has expired"
            description="Please go back to your existing device and choose Start over to try again."
          />
        {:else if registerAccessMethodFlow.view === "unableToComplete"}
          <Alert
            variant="error"
            title="Unable to complete setup"
            description="Please go back to your existing device and choose Start over to try again."
          />
        {/if}
      {/if}
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
