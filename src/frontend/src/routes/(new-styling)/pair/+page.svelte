<script lang="ts">
  import Header from "$lib/components/layout/Header.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { RegisterAccessMethodFlow } from "$lib/flows/registerAccessMethodFlow.svelte.js";
  import { goto } from "$app/navigation";
  import ConfirmYourSignIn from "$lib/components/views/ConfirmYourSignIn.svelte";
  import ConfirmThisDevice from "$lib/components/views/ConfirmThisDevice.svelte";
  import { onMount } from "svelte";
  import { handleError } from "$lib/components/utils/error";
  import { CircleAlertIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";

  const registerAccessMethodFlow = new RegisterAccessMethodFlow();

  const handleCreatePasskey = async () => {
    try {
      await registerAccessMethodFlow.createPasskey();
      await goto("/");
    } catch (error) {
      handleError(error);
    }
  };

  onMount(async () => {
    try {
      await registerAccessMethodFlow.registerTempKey(
        window.location.hash.slice(1),
      );
    } catch (error) {
      handleError(error);
    }
  });
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <AuthPanel class="sm:max-w-100">
      <div class="flex-1"></div>
      {#if registerAccessMethodFlow.view === "confirmDevice"}
        <ConfirmThisDevice
          confirmationCode={registerAccessMethodFlow.confirmationCode}
        />
      {:else if registerAccessMethodFlow.view === "confirmSignIn"}
        <ConfirmYourSignIn
          name={registerAccessMethodFlow.identityName}
          createPasskey={handleCreatePasskey}
        />
      {/if}
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

{#if registerAccessMethodFlow.isUnableToComplete}
  <Dialog>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon size="1.5rem" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      Unable to complete setup
    </h1>
    <p class="text-md text-text-tertiary font-medium">
      Please go back to your <b class="text-text-primary">existing device</b>
      and choose <b class="text-text-primary">Start over</b> to try again.
    </p>
  </Dialog>
{/if}
