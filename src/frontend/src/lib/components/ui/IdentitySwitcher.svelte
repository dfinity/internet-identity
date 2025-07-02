<script lang="ts">
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import {
    UserIcon,
    PlusIcon,
    ArrowUpRightIcon,
    LifeBuoyIcon,
    CodeSquareIcon,
    XIcon,
    LogOutIcon,
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
    selected: bigint;
    identities: LastUsedIdentity[];
    switchIdentity: (identityNumber: bigint) => void;
    useAnotherIdentity: () => void;
    onClose?: () => void;
    onLogout?: () => void;
  };

  const {
    selected,
    identities,
    switchIdentity,
    useAnotherIdentity,
    onClose,
    onLogout,
  }: Props = $props();

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

<div class="mb-4 flex items-center">
  <h2 class="text-text-primary text-md font-medium">Switch identity</h2>
  {#if nonNullish(onClose)}
    <Button
      onclick={onClose}
      variant="tertiary"
      size="sm"
      iconOnly
      class="ml-auto !rounded-full"
      aria-label="Close"
    >
      <XIcon size="1.25rem" />
    </Button>
  {/if}
</div>
<div class="mb-5 flex flex-col gap-1.5">
  <ul class="contents">
    {#each identities as identity}
      <li class="contents">
        <ButtonCard
          onclick={() => switchIdentity(identity.identityNumber)}
          class={[
            selected === identity.identityNumber &&
              "!ring-border-brand !ring-2",
          ]}
        >
          <Avatar size="sm">
            <UserIcon size="1.25rem" />
          </Avatar>
          <div class="flex flex-col text-left text-sm">
            <div class="text-text-primary font-semibold">
              {identity.name ?? identity.identityNumber}
            </div>
            <div class="text-text-tertiary" aria-hidden="true">
              {"passkey" in identity.authMethod ? "Passkey" : "Google"}
            </div>
          </div>
          {#if selected === identity.identityNumber}
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
  {#if onLogout}
    <Button onclick={onLogout} variant="tertiary"
      ><LogOutIcon size="1.25rem" />Sign Out</Button
    >
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
