<script lang="ts">
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import {
    UserIcon,
    XIcon,
    LogOutIcon,
    Settings,
    UserPlusIcon,
  } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import type { HTMLAttributes } from "svelte/elements";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { nonNullish } from "@dfinity/utils";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import { lastUsedIdentityTypeName } from "$lib/utils/lastUsedIdentity";
  import { canisterConfig } from "$lib/globals";

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

  const manageIdentityUrl: string | undefined =
    canisterConfig?.new_flow_origins[0]?.[0];
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
<div class="mb-4 flex flex-col gap-1.5">
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
              {lastUsedIdentityTypeName(identity)}
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
      <!-- Design also uses smaller icon -->
      <!-- It looks better than the same size as the others -->
      <UserPlusIcon size="1rem" />
    </FeaturedIcon>
    <span>Use another identity</span>
  </ButtonCard>
</div>
{#if onLogout}
  <Button onclick={onLogout} variant="tertiary">
    <LogOutIcon size="1.25rem" />
    Sign Out</Button
  >
{:else if nonNullish(manageIdentityUrl)}
  <Button href="/manage" target="_blank" variant="tertiary">
    <Settings size="1.25rem" />
    Manage Identity</Button
  >
{/if}
