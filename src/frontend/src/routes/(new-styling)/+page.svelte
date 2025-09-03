<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { handleError } from "$lib/components/utils/error";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { toaster } from "$lib/components/utils/toaster";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { PlusIcon, UserIcon } from "@lucide/svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { goto } from "$app/navigation";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import type { PageProps } from "./$types";
  import { onMount } from "svelte";
  import { LANDING_PAGE_REDESIGN } from "$lib/state/featureFlags";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { lastUsedIdentityTypeName } from "$lib/utils/lastUsedIdentity";
  import { findConfig, isOpenIdConfig } from "$lib/utils/openID";
  import Button from "$lib/components/ui/Button.svelte";
  import FlickeringGrid from "$lib/components/ui/FlickeringGrid.svelte";
  import { derived } from "svelte/store";
  import { themeStore } from "$lib/stores/theme.store";
  import TextFade from "$lib/components/ui/TextFade.svelte";
  import FullControlIllustration from "$lib/components/illustrations/FullControlIllustration.svelte";
  import EasyAccessIllustration from "$lib/components/illustrations/EasyAccessIllustration.svelte";
  import PasswordFreeIllustration from "$lib/components/illustrations/PasswordFreeIllustration.svelte";
  import LandingCard from "$lib/components/ui/LandingCard.svelte";
  import Accordion from "$lib/components/ui/Accordion.svelte";

  const { data }: PageProps = $props();

  const faq = [
    {
      question: "What is Internet Identity?",
      answer:
        "Internet Identity is a new way to log in without usernames or passwords, otherwise known as decentralized.\n\nInstead of using usernames and passwords (which can be hacked, stolen, or forgotten), Internet Identity lets you log in securely with devices you already own — like your phone, laptop, or a hardware key.",
    },
    {
      question: "How do passkeys work?",
      answer:
        "Internet Identity is a new way to log in without usernames or passwords, otherwise known as decentralized.\n\nInstead of using usernames and passwords (which can be hacked, stolen, or forgotten), Internet Identity lets you log in securely with devices you already own — like your phone, laptop, or a hardware key.",
    },
    {
      question: "What makes Internet Identity secure & easy to use?",
      answer:
        "Internet Identity is a new way to log in without usernames or passwords, otherwise known as decentralized.\n\nInstead of using usernames and passwords (which can be hacked, stolen, or forgotten), Internet Identity lets you log in securely with devices you already own — like your phone, laptop, or a hardware key.",
    },
    {
      question: "What makes Internet Identity privacy-preserving?",
      answer:
        "Internet Identity is a new way to log in without usernames or passwords, otherwise known as decentralized.\n\nInstead of using usernames and passwords (which can be hacked, stolen, or forgotten), Internet Identity lets you log in securely with devices you already own — like your phone, laptop, or a hardware key.",
    },
    {
      question: "How do Web2 integrations work with Internet Identity?",
      answer:
        "Internet Identity is a new way to log in without usernames or passwords, otherwise known as decentralized.\n\nInstead of using usernames and passwords (which can be hacked, stolen, or forgotten), Internet Identity lets you log in securely with devices you already own — like your phone, laptop, or a hardware key.",
    },
  ];

  const flickerColor = derived(themeStore, (isDark) =>
    isDark
      ? "#000000"
      : getComputedStyle(document.documentElement).getPropertyValue(
          "--border-tertiary",
        ),
  );

  const illustrationColours = derived(themeStore, (isDark) =>
    isDark
      ? {
          start: getComputedStyle(document.documentElement).getPropertyValue(
            "--fg-quaternary",
          ),
          end: getComputedStyle(document.documentElement).getPropertyValue(
            "--fg-quaternary_hover",
          ),
        }
      : {
          start: getComputedStyle(document.documentElement).getPropertyValue(
            "--fg-tertiary",
          ),
          end: getComputedStyle(document.documentElement).getPropertyValue(
            "--fg-primary",
          ),
        },
  );

  const gotoNext = () => goto(data.next ?? "/manage", { replaceState: true });

  const onMigration = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    toaster.success({
      title: "Migration completed successfully",
      duration: 4000,
    });
    isAuthDialogOpen = false;
    await goto("/manage");
  };
  const onSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    isAuthDialogOpen = false;
    authenticationV2Funnel.trigger(AuthenticationV2Events.GoToDashboard);
    authenticationV2Funnel.close();
    await gotoNext();
  };
  const onSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: "You're all set. Your identity has been created.",
      duration: 2000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    isAuthDialogOpen = false;
    authenticationV2Funnel.trigger(AuthenticationV2Events.GoToDashboard);
    authenticationV2Funnel.close();
    await gotoNext();
  };

  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore.identities)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );
  const authLastUsedFlow = new AuthLastUsedFlow();
  // Initialize the flow every time the last used identities change
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedIdentities.map(({ identityNumber }) => identityNumber),
    ),
  );

  let isAuthDialogOpen = $state(false);
  let isAuthenticating = $state(false);

  const handleContinueAs = async (identity: LastUsedIdentity) => {
    await authLastUsedFlow.authenticate(identity).catch(handleError);
    if ("passkey" in identity.authMethod) {
      authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsPasskey);
    } else if ("openid" in identity.authMethod) {
      const config = findConfig(
        identity.authMethod.openid.iss,
        identity.authMethod.openid.metadata ?? [],
      );
      if (nonNullish(config) && isOpenIdConfig(config)) {
        authenticationV2Funnel.addProperties({
          provider: config.name,
        });
      }
      authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsOpenID);
    }
    await onSignIn(identity.identityNumber);
  };

  onMount(() => {
    authenticationV2Funnel.init({ origin: window.location.origin });
  });
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  {#if $LANDING_PAGE_REDESIGN}
    <Header>
      <div class="flex flex-1 flex-row items-center justify-end gap-5">
        <Button variant="secondary">For developers</Button>
        <Button variant="primary">Manage Identity</Button>
      </div>
    </Header>
    <div class="flex h-[512px] w-full flex-row">
      <div class="relative w-1/4 overflow-hidden">
        <FlickeringGrid color={$flickerColor} />
      </div>
      <div class="flex w-1/2 flex-col items-center justify-center gap-6">
        <div class="flex w-full flex-col gap-2">
          <h1 class="text-text-disabled text-center text-7xl">Experience</h1>
          <TextFade
            texts={[
              "Full Privacy",
              "Ownership",
              "Control",
              "Internet Identity",
            ]}
            duration={500}
            delay={2000}
            className="text-7xl text-text-primary"
            containerClass="h-[80px] w-full flex items-center justify-center"
          />
        </div>
        <p class="text-text-tertiary max-w-[534px] text-center text-base">
          Internet Identity lets you access apps and services securely, without
          creating passwords, sharing personal data, or giving up control.
        </p>
      </div>
      <div class="relative w-1/4 overflow-hidden">
        <FlickeringGrid color={$flickerColor} />
      </div>
    </div>
    <div class="mx-auto flex flex-row gap-4 px-8 pb-8">
      <LandingCard
        header="EASY ACCESS"
        subheader="Sign in with multiple providers"
        description="Make sign-up simple by connecting your Google, Apple, or Microsoft account - the familiar way to log in, now powered by decentralized identity."
      >
        <EasyAccessIllustration
          slot="illustration"
          class="w-[233px]"
          colors={$illustrationColours}
        />
      </LandingCard>
      <LandingCard
        header="PASSWORD-FREE"
        subheader="Discoverable passkeys"
        description="Forget about remembering identity numbers or complicated passwords. With passkeys, you simply pick your name to log in — quick, safe, and hassle-free."
      >
        <PasswordFreeIllustration
          slot="illustration"
          colors={$illustrationColours}
          class="w-[264px]"
        />
      </LandingCard>
      <LandingCard
        header="FULL CONTROL"
        subheader="Advanced identity management"
        description="Manage your identities and stay in control of your dApps and websites with your dashboard. Explore Pro Features to further customize and secure your experience."
      >
        <FullControlIllustration
          slot="illustration"
          colors={$illustrationColours}
          class="w-[170px]"
        />
      </LandingCard>
    </div>
    <div class="mx-auto flex max-w-7xl flex-row gap-16 px-16 py-24">
      <div class="flex flex-1 flex-col gap-5">
        <div class="flex flex-col gap-3">
          <p class="text-text-tertiary text-sm">
            Understanding Internet Identity
          </p>
          <p class="text-text-primary text-4xl">FAQs</p>
        </div>
        <p class="text-text-tertiary text-lg">
          Everything you need to know about Internet Identity and how your
          privacy is protected, secure, decentralized and managed only by you.
        </p>
      </div>
      <div class="flex flex-1 flex-col gap-8">
        {#each faq as item, i}
          <div class="flex flex-col gap-6">
            {#if i > 0 && i < faq.length}
              <div class="border-border-secondary border-t"></div>
            {/if}
            <Accordion header={item.question}>
              <p class="text-text-secondary text-base whitespace-pre-line">
                {item.answer}
              </p>
            </Accordion>
          </div>
        {/each}
      </div>
    </div>
    <Footer />
  {:else}
    <Header />
    <div class="flex flex-1 flex-col items-center justify-center">
      <AuthPanel class="sm:max-w-100">
        <div class="flex-1"></div>
        {#if lastUsedIdentities.length > 0}
          <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
            Manage your Internet&nbsp;Identity
          </h1>
          <p class="text-text-secondary mb-6 self-start text-sm">
            choose identity to continue
          </p>
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
                        {lastUsedIdentityTypeName(identity)}
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
            <Dialog
              onClose={() => (isAuthDialogOpen = false)}
              showCloseButton={!isAuthenticating}
              closeOnOutsideClick={!isAuthenticating}
            >
              <AuthWizard
                bind:isAuthenticating
                {onSignIn}
                {onSignUp}
                {onMigration}
                onOtherDevice={() => (isAuthDialogOpen = false)}
                onError={(error) => {
                  isAuthDialogOpen = false;
                  handleError(error);
                }}
                withinDialog
              >
                <h1
                  class="text-text-primary my-2 self-start text-2xl font-medium"
                >
                  Use another identity
                </h1>
                <p class="text-text-secondary mb-6 self-start text-sm">
                  choose method
                </p>
              </AuthWizard>
            </Dialog>
          {/if}
        {:else}
          <AuthWizard {onSignIn} {onSignUp} {onMigration} onError={handleError}>
            <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
              Manage your Internet&nbsp;Identity
            </h1>
            <p class="text-text-secondary mb-6 self-start text-sm">
              sign in to continue
            </p>
          </AuthWizard>
        {/if}
      </AuthPanel>
    </div>
    <Footer />
  {/if}
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
