<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { handleError } from "$lib/components/utils/error";
  import PickAuthenticationMethod from "$lib/components/views/PickAuthenticationMethod.svelte";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { toaster } from "$lib/components/utils/toaster";
  import SolveCaptcha from "$lib/components/views/SolveCaptcha.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/views/CreatePasskey.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { PlusIcon, UserIcon } from "@lucide/svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import AuthDialog from "$lib/components/views/AuthDialog.svelte";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { goto } from "$app/navigation";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import FlairCanvas from "$lib/components/backgrounds/FlairCanvas.svelte";
  import { type FlairAnimationOptions } from "$lib/components/backgrounds/FlairCanvas";

  const gotoManage = () => goto("/manage", { replaceState: true });
  const onSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    if (triggerAnimation) {
      triggerAnimation({
        location: "center",
        target: ["motion"],

        motionType: "omni",
        intensity: "light",
        speed: 5,
        nImpulses: "single",
        size: "large",
        impulseEasing: "cubicIn",
      });
    }
    isAuthDialogOpen = false;
    setTimeout(async () => {
      await gotoManage();
    }, 700);
  };
  const onSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: "You're all set. Your identity has been created.",
      duration: 2000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    if (triggerAnimation) {
      triggerAnimation({
        location: "center",
        target: ["motion"],

        motionType: "omni",
        intensity: "light",
        speed: 5,
        nImpulses: "single",
        size: "large",
        impulseEasing: "cubicIn",
      });
    }
    isAuthDialogOpen = false;
    setTimeout(async () => {
      await gotoManage();
    }, 700);
  };
  const authFlow = new AuthFlow({ onSignIn, onSignUp });
  const authLastUsedFlow = new AuthLastUsedFlow();

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );

  let isAuthDialogOpen = $state(false);

  const handleContinueAs = async (identity: LastUsedIdentity) => {
    if (triggerAnimation) {
      triggerAnimation({
        location: "center",
        target: ["motion"],

        motionType: "omni",
        intensity: "light",
        speed: 5,
        nImpulses: "single",
        size: "large",
        impulseEasing: "cubicIn",
      });
    }
    await authLastUsedFlow.authenticate(identity).catch(handleError);
    await onSignIn(identity.identityNumber);
  };

  let triggerAnimation = $state<(opts: FlairAnimationOptions) => void>();
</script>

<div class="flex min-h-[100dvh] flex-col">
  <FlairCanvas
    spacing="medium"
    aspect="ultrawide"
    dotSize={1.1}
    vignette="center"
    bind:triggerAnimation
  />
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <AuthPanel class="z-10 sm:max-w-100">
      <div class="flex-1"></div>
      {#if nonNullish(authFlow.captcha)}
        <SolveCaptcha {...authFlow.captcha} />
      {:else}
        <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
          {lastUsedIdentities.length > 0 ? "Choose identity" : "Sign in"}
        </h1>
        <p class="text-text-secondary mb-6 self-start text-sm">
          {lastUsedIdentities.length > 0
            ? "you want to manage"
            : "to manage your identity"}
        </p>
        {#if lastUsedIdentities.length > 0}
          <div class="flex flex-col gap-1.5">
            <ul class="contents">
              {#each lastUsedIdentities as identity}
                <li class="contents">
                  <ButtonCard
                    onclick={() => handleContinueAs(identity)}
                    disabled={nonNullish(
                      authLastUsedFlow.authenticatingIdentity,
                    )}
                  >
                    <Avatar size="sm">
                      {#if identity.identityNumber === authLastUsedFlow.authenticatingIdentity}
                        <ProgressRing />
                      {:else}
                        <UserIcon size="1.25rem" />
                      {/if}
                    </Avatar>
                    <div class="flex flex-col text-left text-sm">
                      <div class="font-semibold">
                        {identity.name ?? identity.identityNumber}
                      </div>
                      <div class="text-text-tertiary" aria-hidden="true">
                        {"passkey" in identity.authMethod
                          ? "Passkey"
                          : "Google"}
                      </div>
                    </div>
                  </ButtonCard>
                </li>
              {/each}
            </ul>
            <ButtonCard
              onclick={() => (isAuthDialogOpen = true)}
              disabled={nonNullish(authLastUsedFlow.authenticatingIdentity)}
            >
              <FeaturedIcon size="sm">
                <PlusIcon size="1.25rem" />
              </FeaturedIcon>
              <span>Use another identity</span>
            </ButtonCard>
          </div>
          {#if isAuthDialogOpen}
            <AuthDialog
              {onSignIn}
              {onSignUp}
              title="Use another identity"
              subtitle="Choose method"
              onClose={() => (isAuthDialogOpen = false)}
            />
          {/if}
        {:else}
          <PickAuthenticationMethod
            setupOrUseExistingPasskey={authFlow.setupOrUseExistingPasskey}
            continueWithGoogle={authFlow.continueWithGoogle}
          />
          {#if authFlow.view !== "chooseMethod"}
            <Dialog onClose={() => (authFlow.view = "chooseMethod")}>
              {#if authFlow.view === "setupOrUseExistingPasskey"}
                <SetupOrUseExistingPasskey
                  setupNew={authFlow.setupNewPasskey}
                  useExisting={authFlow.continueWithExistingPasskey}
                />
              {:else if authFlow.view === "setupNewPasskey"}
                <CreatePasskey create={authFlow.createPasskey} />
              {/if}
            </Dialog>
          {/if}
        {/if}
      {/if}
      {#if authFlow.systemOverlay || authLastUsedFlow.systemOverlay}
        <SystemOverlayBackdrop />
      {/if}
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
