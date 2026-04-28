<script lang="ts">
  import {
    XIcon,
    LogOutIcon,
    ArrowRightIcon,
    PlusIcon,
    PencilIcon,
  } from "@lucide/svelte";
  import { SvelteMap } from "svelte/reactivity";
  import type { HTMLAttributes } from "svelte/elements";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { getMetadataString } from "$lib/utils/openID";
  import IdentityAvatar from "./IdentityAvatar.svelte";
  import IdentityListItem from "./IdentityListItem.svelte";

  type Props = HTMLAttributes<HTMLElement> & {
    selected: bigint;
    identities: LastUsedIdentity[];
    onSwitchIdentity: (identityNumber: bigint) => Promise<void>;
    onUseAnotherIdentity: () => void;
    onManageIdentity?: () => Promise<void>;
    onManageIdentities?: () => void;
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
    onManageIdentities,
    onError,
    onClose,
    onSignOut,
  }: Props = $props();

  // Snapshot identities on render to prevent UI changes while popover is open.
  // Popover shows up-to-date info when opened, but stays stable during interaction.
  let initialIdentities = $state(identities);
  let initialSelected = $state(selected);

  let switchingToIdentity = $state<bigint>();
  let isNavigatingToManage = $state(false);
  let isSigningOut = $state(false);
  let windowHeight = $state(window.innerHeight);

  const selectedIdentity = $derived(
    initialIdentities.find(
      (identity) => identity.identityNumber === initialSelected,
    )!,
  );
  const otherIdentities = $derived(
    initialIdentities.filter(
      (identity) => identity.identityNumber !== initialSelected,
    ),
  );
  const passkeyNameCounts = $derived.by(() => {
    const counts = new SvelteMap<string | undefined, number>();

    for (const identity of otherIdentities) {
      if (!("passkey" in identity.authMethod)) {
        continue;
      }

      counts.set(identity.name, (counts.get(identity.name) ?? 0) + 1);
    }

    return counts;
  });

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
    try {
      isNavigatingToManage = true;
      await onManageIdentity?.();
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

{#snippet selectedIdentityCard()}
  <div
    class="bg-bg-secondary border-border-secondary relative -mx-px -my-px flex flex-col items-center rounded-b-2xl border-x border-b p-8"
  >
    <div class="mb-2">
      <IdentityAvatar identity={selectedIdentity} size="lg" />
    </div>
    <p
      class="text-text-primary max-w-full overflow-hidden text-sm font-semibold text-ellipsis whitespace-nowrap"
    >
      {selectedIdentity.name ?? selectedIdentity.identityNumber}
    </p>
    <p
      class="text-text-tertiary mb-6 max-w-full overflow-hidden text-sm text-ellipsis whitespace-nowrap"
    >
      {#if "openid" in selectedIdentity.authMethod && selectedIdentity.authMethod.openid.metadata !== undefined}
        <span
          >{getMetadataString(
            selectedIdentity.authMethod.openid.metadata,
            "email",
          ) ?? $t`Hidden email`}</span
        >
      {:else if "sso" in selectedIdentity.authMethod}
        {@const sso = selectedIdentity.authMethod.sso}
        <span>{sso.email ?? sso.name ?? sso.domain}</span>
      {/if}
    </p>
    {#if onSignOut !== undefined}
      <button onclick={handleSignOut} class="btn btn-secondary w-full">
        <LogOutIcon class="size-4" />
        {$t`Sign out`}
      </button>
    {/if}
    {#if onManageIdentity !== undefined}
      <button
        onclick={handleManageIdentity}
        class="btn btn-secondary group w-full gap-2.5"
      >
        {#if isNavigatingToManage}
          <ProgressRing class="size-4" />
        {/if}
        {$t`Manage your Internet Identity`}
      </button>
    {/if}
    <button
      onclick={onClose}
      class="btn btn-tertiary btn-sm btn-icon absolute inset-e-2 top-2 rounded-full"
    >
      <XIcon class="size-5" />
      <span>{$t`Close`}</span>
    </button>
  </div>
{/snippet}

{#snippet identityListItem(identity: LastUsedIdentity)}
  {@const notUnique =
    "passkey" in identity.authMethod &&
    (passkeyNameCounts.get(identity.name) ?? 0) > 1}
  <li class="mx-4">
    <button
      onclick={() => handleSwitchIdentity(identity.identityNumber)}
      class={[
        "group flex w-full flex-row items-center gap-3 p-3 text-start",
        "border-border-secondary rounded-md border outline-none",
        "enabled:hover:bg-bg-primary_hover enabled:focus-visible:bg-bg-primary_hover",
        "disabled:border-border-disabled",
      ]}
    >
      <IdentityListItem {identity} showCreatedAt={notUnique} />
      {#if switchingToIdentity === identity.identityNumber}
        <ProgressRing class="text-fg-disabled ms-auto size-5" />
      {:else}
        <ArrowRightIcon
          class={[
            "text-fg-tertiary ms-auto me-1 size-5 transform opacity-0 transition-all duration-200 rtl:-scale-x-100",
            "group-enabled:group-hover:me-0 group-enabled:group-hover:opacity-100",
            "group-enabled:group-focus-visible:me-0 group-enabled:group-focus-visible:opacity-100",
          ]}
        />
      {/if}
    </button>
  </li>
{/snippet}

{#snippet otherIdentitiesList()}
  <div>
    <div class="mt-4 mb-2 flex h-9 flex-row items-center">
      <h2 class="text-text-primary mx-4 text-sm font-semibold">
        {$t`Sign in with another identity`}
      </h2>
      {#if onManageIdentities !== undefined}
        <button
          onclick={onManageIdentities}
          class="btn btn-tertiary btn-icon btn-sm ms-auto me-2 rounded-full"
        >
          <PencilIcon class="size-4" />
          <span>{$t`Edit`}</span>
        </button>
      {/if}
    </div>
    <ul
      class="flex flex-col gap-2 overflow-y-auto"
      style={`max-height: ${Math.max(2, Math.floor((windowHeight - 380) / 74)) * 74 - 41}px`}
    >
      {#each otherIdentities as identity (identity.identityNumber)}
        {@render identityListItem(identity)}
      {/each}
    </ul>
  </div>
{/snippet}

<svelte:window bind:innerHeight={windowHeight} />

<fieldset
  disabled={switchingToIdentity !== undefined ||
    isNavigatingToManage ||
    isSigningOut}
  class="contents"
>
  <div class="flex flex-col overflow-x-hidden">
    {#if selectedIdentity !== undefined}
      {@render selectedIdentityCard()}
    {/if}
    {#if otherIdentities.length > 0}
      {@render otherIdentitiesList()}
    {/if}
    <button onclick={onUseAnotherIdentity} class="btn btn-tertiary m-4">
      <PlusIcon class="size-4" />
      {$t`Add another identity`}
    </button>
  </div>
</fieldset>
