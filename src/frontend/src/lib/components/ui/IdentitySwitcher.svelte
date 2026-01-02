<script lang="ts">
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import {
    UserIcon,
    XIcon,
    LogOutIcon,
    Settings,
    UserPlusIcon,
  } from "@lucide/svelte";
  import type { HTMLAttributes } from "svelte/elements";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { nonNullish } from "@dfinity/utils";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import { lastUsedIdentityTypeName } from "$lib/utils/lastUsedIdentity";
  import { canisterConfig } from "$lib/globals";
  import { formatDate, t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  type Props = HTMLAttributes<HTMLElement> & {
    selected: bigint;
    identities: LastUsedIdentity[];
    onSwitchIdentity: (identityNumber: bigint) => Promise<void>;
    onUseAnotherIdentity: () => void;
    onManageIdentity?: () => Promise<void>;
    onError: (error: unknown) => void;
    onClose: () => void;
    onSignOut?: () => Promise<void>;
  };

  const {
    selected,
    identities,
    onSwitchIdentity,
    onUseAnotherIdentity,
    onManageIdentity,
    onError,
    onClose,
    onSignOut,
  }: Props = $props();

  const manageIdentityUrl: string | undefined =
    canisterConfig?.new_flow_origins[0]?.[0];

  let switchingToIdentity = $state<bigint>();
  let isNavigatingToManage = $state(false);
  let isSigningOut = $state(false);

  const handleSwitchIdentity = async (identityNumber: bigint) => {
    try {
      switchingToIdentity = identityNumber;
      await onSwitchIdentity(identityNumber);
    } catch (error) {
      onError(error);
    } finally {
      switchingToIdentity = undefined;
    }
  };

  const handleManageIdentity = async () => {
    if (onManageIdentity === undefined) {
      window.open(manageIdentityUrl, "_blank");
      return;
    }
    try {
      isNavigatingToManage = true;
      await onManageIdentity();
    } catch (error) {
      onError(error);
    } finally {
      isNavigatingToManage = false;
    }
  };

  const handleSignOut = async () => {
    try {
      isSigningOut = true;
      await onSignOut?.();
    } catch (error) {
      onError(error);
    } finally {
      isSigningOut = false;
    }
  };
</script>

<fieldset
  disabled={switchingToIdentity !== undefined ||
    isNavigatingToManage ||
    isSigningOut}
  class="contents"
>
  <div class="mb-4 flex items-center">
    <h2 class="text-text-primary text-base font-medium">
      {$t`Switch identity`}
    </h2>
    <button
      onclick={onClose}
      class="btn btn-tertiary btn-sm btn-icon ms-auto !rounded-full"
      aria-label={$t`Close`}
    >
      <XIcon class="size-5" />
    </button>
  </div>
  <div class="mb-4 flex flex-col gap-1.5">
    <ul class="contents">
      {#each identities as identity}
        {@const name = lastUsedIdentityTypeName(identity)}
        {@const date = nonNullish(identity.createdAtMillis)
          ? $formatDate(new Date(identity.createdAtMillis), {
              dateStyle: "short",
            })
          : undefined}
        <li class="contents">
          <ButtonCard
            onclick={() => handleSwitchIdentity(identity.identityNumber)}
            class={[
              selected === identity.identityNumber &&
                "!ring-border-brand disabled:!ring-border-disabled !ring-2",
            ]}
          >
            <Avatar size="sm">
              {#if switchingToIdentity === identity.identityNumber}
                <ProgressRing />
              {:else}
                <UserIcon class="size-5" />
              {/if}
            </Avatar>
            <div class="flex flex-col text-left text-sm">
              <div class="text-text-primary font-semibold">
                {identity.name ?? identity.identityNumber}
              </div>
              <div class="text-text-tertiary font-normal" aria-hidden="true">
                {#if nonNullish(date)}
                  <span>{$t`${name} | Created ${date}`}</span>
                {:else}
                  <span>{name}</span>
                {/if}
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
    <ButtonCard onclick={onUseAnotherIdentity}>
      <FeaturedIcon size="sm">
        <!-- Design also uses smaller icon -->
        <!-- It looks better than the same size as the others -->
        <UserPlusIcon class="size-4" />
      </FeaturedIcon>
      <span>{$t`Use another identity`}</span>
    </ButtonCard>
  </div>
  {#if onSignOut !== undefined}
    <button onclick={handleSignOut} class="btn btn-tertiary gap-2">
      {#if isSigningOut}
        <ProgressRing />
      {:else}
        <LogOutIcon class="size-5" />
      {/if}
      <span>{$t`Sign Out`}</span>
    </button>
  {:else}
    <button onclick={handleManageIdentity} class="btn btn-tertiary gap-2">
      {#if isNavigatingToManage}
        <ProgressRing />
      {:else}
        <Settings class="size-5" />
      {/if}
      <span>{$t`Manage Identity`}</span>
    </button>
  {/if}
</fieldset>
