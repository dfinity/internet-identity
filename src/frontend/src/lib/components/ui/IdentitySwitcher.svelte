<script lang="ts">
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import {
    UserIcon,
    XIcon,
    LogOutIcon,
    ArrowRightIcon,
    PlusIcon,
  } from "@lucide/svelte";
  import type { HTMLAttributes } from "svelte/elements";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { getMetadataString, openIdLogo } from "$lib/utils/openID";
  import PasskeyIcon from "../icons/PasskeyIcon.svelte";

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
    ),
  );
  const selectedLogo = $derived(
    selectedIdentity !== undefined &&
      "openid" in selectedIdentity.authMethod &&
      selectedIdentity.authMethod.openid.metadata !== undefined
      ? openIdLogo(
          selectedIdentity.authMethod.openid.iss,
          selectedIdentity.authMethod.openid.metadata,
        )
      : undefined,
  );
  const otherIdentities = $derived(
    initialIdentities.filter(
      (identity) => identity.identityNumber !== initialSelected,
    ),
  );

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

<svelte:window bind:innerHeight={windowHeight} />

<fieldset
  disabled={switchingToIdentity !== undefined ||
    isNavigatingToManage ||
    isSigningOut}
  class="contents"
>
  <div class="flex flex-col overflow-x-hidden">
    {#if selectedIdentity !== undefined}
      <div
        class="bg-bg-secondary border-border-secondary relative mx-[-1px] my-[-1px] flex flex-col items-center rounded-b-2xl border-x border-b p-8"
      >
        <div class="relative mb-2">
          <Avatar size="lg">
            <UserIcon class="size-6" />
          </Avatar>
          <div
            class="bg-bg-primary_alt border-border-secondary absolute -right-1 -bottom-1 flex size-6.5 items-center justify-center rounded-full border"
          >
            {#if selectedLogo !== undefined}
              <div class="text-fg-tertiary size-4.25">
                {@html selectedLogo}
              </div>
            {:else}
              <PasskeyIcon class="text-fg-tertiary !size-4.25" />
            {/if}
          </div>
        </div>
        <div
          class="text-text-primary max-w-full overflow-hidden text-sm font-semibold text-ellipsis whitespace-nowrap"
        >
          {selectedIdentity.name ?? selectedIdentity.identityNumber}
        </div>
        <div
          class="text-text-tertiary mb-6 max-w-full overflow-hidden text-sm text-ellipsis whitespace-nowrap"
        >
          {#if "openid" in selectedIdentity.authMethod}
            <span
              >{getMetadataString(
                selectedIdentity.authMethod.openid.metadata!,
                "email",
              ) ?? $t`Hidden email`}</span
            >
          {:else}
            <span>
              {$t`Passkey`}
            </span>
          {/if}
        </div>
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
          class="btn btn-tertiary btn-sm btn-icon absolute end-2 top-2 !rounded-full"
          aria-label={$t`Close`}
        >
          <XIcon class="size-5" />
        </button>
      </div>
    {/if}
    {#if otherIdentities.length > 0}
      <div class="text-text-primary mx-4 mt-6 mb-4 text-sm font-semibold">
        {$t`Sign in with another identity`}
      </div>
      <div
        class="flex flex-col gap-2 overflow-y-auto"
        style={`max-height: ${Math.max(2, Math.floor((windowHeight - 380) / 74)) * 74 - 41}px`}
      >
        {#each otherIdentities as identity}
          {@const logo =
            "openid" in identity.authMethod &&
            identity.authMethod.openid.metadata !== undefined
              ? openIdLogo(
                  identity.authMethod.openid.iss,
                  identity.authMethod.openid.metadata,
                )
              : undefined}
          {@const notUnique =
            otherIdentities.filter(
              (otherIdentity) =>
                "passkey" in identity.authMethod &&
                "passkey" in otherIdentity.authMethod &&
                identity.name === otherIdentity.name,
            ).length > 1}
          <button
            onclick={() => handleSwitchIdentity(identity.identityNumber)}
            class={[
              "group mx-4 flex flex-row items-center gap-3 p-3 text-start",
              "border-border-secondary rounded-md border",
              "enabled:hover:bg-bg-primary_hover",
              "disabled:border-border-disabled",
            ]}
          >
            <div class="relative">
              <Avatar size="sm">
                <UserIcon class="size-5" />
              </Avatar>
              <div
                class="bg-bg-primary_alt border-border-secondary absolute -right-1.25 -bottom-1.25 flex size-5 items-center justify-center rounded-full border"
              >
                {#if logo !== undefined}
                  <div class="text-fg-tertiary size-3.25">
                    {@html logo}
                  </div>
                {:else}
                  <PasskeyIcon class="text-fg-tertiary !size-3" />
                {/if}
              </div>
            </div>
            <div
              class="flex flex-col overflow-hidden group-disabled:opacity-50"
            >
              <div class="text-text-primary text-sm font-semibold">
                {identity.name ?? identity.identityNumber}
              </div>
              <div
                class="text-text-tertiary overflow-hidden text-sm text-ellipsis whitespace-nowrap"
              >
                {#if "openid" in identity.authMethod}
                  <span
                    >{getMetadataString(
                      identity.authMethod.openid.metadata!,
                      "email",
                    ) ?? $t`Hidden email`}</span
                  >
                {:else}
                  <span>
                    {$t`Passkey`}
                    {#if notUnique && identity.createdAtMillis !== undefined}
                      {" | "}
                      {$t`Created ${$formatRelative(
                        new Date(identity.createdAtMillis),
                        {
                          style: "long",
                        },
                      )}`}
                    {/if}
                  </span>
                {/if}
              </div>
            </div>
            {#if switchingToIdentity === identity.identityNumber}
              <ProgressRing class="text-fg-disabled ms-auto size-5" />
            {:else}
              <ArrowRightIcon
                class={[
                  "text-fg-tertiary ms-auto mr-1 size-5 opacity-0 transition-all duration-200",
                  "group-enabled:group-hover:mr-0 group-enabled:group-hover:opacity-100",
                ]}
              />
            {/if}
          </button>
        {/each}
      </div>
    {/if}
    <button onclick={onUseAnotherIdentity} class="btn btn-tertiary m-4">
      <PlusIcon class="size-4" />
      {$t`Add another identity`}
    </button>
  </div>
</fieldset>
