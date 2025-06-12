<script lang="ts">
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import {
    UserIcon,
    PlusIcon,
    ArrowUpRightIcon,
    LifeBuoyIcon,
    CodeSquareIcon,
    XIcon,
    ArrowLeftIcon,
  } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import type { HTMLAttributes } from "svelte/elements";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import {
    INTERNET_COMPUTER_URL,
    SOURCE_CODE_URL,
    SUPPORT_URL,
  } from "$lib/config";
  import { nonNullish } from "@dfinity/utils";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";

  type Props = HTMLAttributes<HTMLElement> & {
    currentIdentityNumber: bigint;
    identities: LastUsedIdentity[];
    switchIdentity: (identityNumber: bigint) => void;
    useAnotherIdentity: () => void;
    onClose?: () => void;
  };

  const {
    currentIdentityNumber,
    identities,
    switchIdentity,
    useAnotherIdentity,
    onClose,
  }: Props = $props();

  const currentIdentity = $derived(
    identities.find(
      (identity) => identity.identityNumber === currentIdentityNumber,
    )!,
  );
  let view = $state<"currentIdentity" | "switchIdentity">("currentIdentity");
  const links = [
    {
      href: INTERNET_COMPUTER_URL,
      label: "Internet Computer",
      icon: ArrowUpRightIcon,
    },
    {
      href: SUPPORT_URL,
      label: "Support",
      icon: LifeBuoyIcon,
    },
    {
      href: SOURCE_CODE_URL,
      label: "Source code",
      icon: CodeSquareIcon,
    },
  ];
</script>

{#snippet currentIdentityView()}
  <div class="mb-4 flex items-center">
    <h2 class="text-text-primary text-md font-medium">Internet Identity</h2>
    {#if nonNullish(onClose)}
      <Button
        onclick={onClose}
        variant="tertiary"
        size="sm"
        iconOnly
        class="ml-auto !rounded-full"
      >
        <XIcon size="1.25rem" />
      </Button>
    {/if}
  </div>
  <div class="mb-10 flex flex-col gap-2">
    <div
      class="bg-bg-brand-primary_alt border-border-secondary flex items-center gap-3 rounded-xl border p-3"
    >
      <Avatar size="sm">
        <UserIcon size="1.25rem" />
      </Avatar>
      <div class="flex flex-1 flex-col text-sm">
        <div class="text-text-primary font-semibold">
          {currentIdentity.name ?? currentIdentity.identityNumber}
        </div>
        <div class="text-text-tertiary">
          {"passkey" in currentIdentity.authMethod ? "Passkey" : "Google"}
        </div>
      </div>
      <Button variant="tertiary" size="sm" class="hidden">Manage</Button>
    </div>
    {#if identities.length > 1}
      <Button onclick={() => (view = "switchIdentity")} variant="secondary">
        <span>Switch identity</span>
      </Button>
    {:else}
      <Button onclick={useAnotherIdentity} variant="secondary">
        <span>Use another identity</span>
      </Button>
    {/if}
  </div>
  <hr class="border-t-border-tertiary mb-4" />
  <div class="flex gap-4">
    {#each links as { href, label }}
      <a
        {href}
        target="_blank"
        class="text-text-secondary text-sm font-medium outline-0 focus-visible:underline"
      >
        {label}
      </a>
    {/each}
  </div>
{/snippet}

{#snippet switchIdentityView()}
  <div class="mb-4 flex items-center gap-2">
    <Button
      onclick={() => (view = "currentIdentity")}
      variant="tertiary"
      size="sm"
      iconOnly
      class="!rounded-full"
    >
      <ArrowLeftIcon size="1.25rem" />
    </Button>
    <h2 class="text-text-primary text-md font-medium">Switch identity</h2>
    {#if nonNullish(onClose)}
      <Button
        onclick={onClose}
        variant="tertiary"
        size="sm"
        iconOnly
        class="ml-auto !rounded-full"
      >
        <XIcon size="1.25rem" />
      </Button>
    {/if}
  </div>
  <div class="flex flex-col gap-1.5">
    <ul class="contents">
      {#each identities as identity}
        <li class="contents">
          <ButtonCard
            onclick={() => switchIdentity(identity.identityNumber)}
            class={[
              currentIdentityNumber === identity.identityNumber &&
                "!ring-border-brand !ring-2",
            ]}
          >
            <Avatar size="sm">
              <UserIcon size="1.25rem" />
            </Avatar>
            <span>{identity.name ?? identity.identityNumber}</span>
            {#if currentIdentityNumber === identity.identityNumber}
              <Checkbox
                checked
                size="md"
                class="pointer-events-none mr-1 ml-auto !rounded-full"
                tabindex={-1}
                aria-hidden
              />
            {/if}
          </ButtonCard>
        </li>
      {/each}
    </ul>
    <ButtonCard onclick={useAnotherIdentity}>
      <FeaturedIcon size="sm">
        <PlusIcon size="1.25rem" />
      </FeaturedIcon>
      <span>Use another identity</span>
    </ButtonCard>
  </div>
{/snippet}

{#if view === "currentIdentity"}
  {@render currentIdentityView()}
{:else if view === "switchIdentity"}
  {@render switchIdentityView()}
{/if}
