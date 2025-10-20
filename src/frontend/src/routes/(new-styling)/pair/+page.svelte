<script lang="ts">
  import Header from "$lib/components/layout/Header.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { goto } from "$app/navigation";
  import { CircleAlertIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { RegisterAccessMethodWizard } from "$lib/components/wizards/registerAccessMethod";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { toaster } from "$lib/components/utils/toaster";

  let isUnableToComplete = $state(false);

  const onRegistered = (identityNumber: bigint) => {
    toaster.success({
      title: "You're all set. Your passkey has been registered.",
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    goto("/manage");
  };
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <AuthPanel class="sm:max-w-100">
      <div class="flex-1"></div>
      <RegisterAccessMethodWizard
        registrationId={window.location.hash.slice(1)}
        {onRegistered}
        onError={() => (isUnableToComplete = true)}
      />
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

{#if isUnableToComplete}
  <Dialog>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
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
