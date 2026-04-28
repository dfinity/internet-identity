<script lang="ts">
  import { SvelteMap } from "svelte/reactivity";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { PencilIcon, Trash2Icon } from "@lucide/svelte";
  import { openIdName } from "$lib/utils/openID";
  import FeaturedIcon from "./FeaturedIcon.svelte";
  import IdentityListItem from "./IdentityListItem.svelte";
  import Tooltip from "./Tooltip.svelte";

  type Props = {
    selected?: bigint;
    identities: LastUsedIdentity[];
    onRemoveIdentity: (identityNumber: bigint) => void;
  };

  let { selected, identities, onRemoveIdentity }: Props = $props();

  let removingIdentity = $state<LastUsedIdentity>();

  const passkeyNameCounts = $derived.by(() => {
    const counts = new SvelteMap<string | undefined, number>();
    for (const identity of identities) {
      if (!("passkey" in identity.authMethod)) {
        continue;
      }
      counts.set(identity.name, (counts.get(identity.name) ?? 0) + 1);
    }
    return counts;
  });

  const handleConfirmRemove = () => {
    if (removingIdentity === undefined) {
      return;
    }
    onRemoveIdentity(removingIdentity.identityNumber);
    removingIdentity = undefined;
  };
</script>

{#snippet removeConfirmation(identity: LastUsedIdentity)}
  {@const provider =
    "sso" in identity.authMethod
      ? (identity.authMethod.sso.name ?? identity.authMethod.sso.domain)
      : "openid" in identity.authMethod &&
          identity.authMethod.openid.metadata !== undefined
        ? openIdName(
            identity.authMethod.openid.iss,
            // `aud` not tracked on `LastUsedIdentity`; see #3795. Fall
            // through to issuer-only `findConfig` — correct for direct
            // providers. SSO entries take the branch above.
            undefined,
            identity.authMethod.openid.metadata,
            undefined,
            undefined,
          )
        : undefined}
  <div class="flex flex-col gap-8">
    <div class="flex flex-col gap-4">
      <FeaturedIcon size="lg">
        <Trash2Icon class="size-6" />
      </FeaturedIcon>
      <div class="flex flex-col gap-3">
        <h2 class="text-text-primary text-2xl font-medium">
          {$t`Remove from this device`}
        </h2>
        <p class="text-text-tertiary text-base">
          {#if provider !== undefined}
            <Trans>
              You can add it back anytime by signing in with the same {provider}
              account.
            </Trans>
          {:else}
            <Trans>
              You can add it back anytime by signing in with the same passkey.
            </Trans>
          {/if}
        </p>
      </div>
    </div>

    <div class="bg-bg-primary_hover flex items-center gap-3 rounded-md p-3">
      <IdentityListItem
        {identity}
        showCreatedAt={(passkeyNameCounts.get(identity.name) ?? 0) > 1}
      />
    </div>

    <div class="flex flex-col gap-3">
      <button onclick={handleConfirmRemove} class="btn btn-danger w-full">
        {$t`Remove`}
      </button>
      <button
        onclick={() => (removingIdentity = undefined)}
        class="btn btn-tertiary w-full"
      >
        {$t`Cancel`}
      </button>
    </div>
  </div>
{/snippet}

{#snippet identityList()}
  <div class="flex flex-col gap-8">
    <div class="flex flex-col gap-4">
      <FeaturedIcon size="lg">
        <PencilIcon class="size-6" />
      </FeaturedIcon>
      <div class="flex flex-col gap-3">
        <h2 class="text-text-primary text-2xl font-medium">
          {$t`Identities on this device`}
        </h2>
        <p class="text-text-tertiary text-base">
          <Trans>
            Remove identities you don't use. You can add them back anytime by
            signing in again.
          </Trans>
        </p>
      </div>
    </div>

    <ul class="flex flex-col">
      {#each identities as identity, i (identity.identityNumber)}
        <li class="flex items-center gap-3 py-3">
          <IdentityListItem
            {identity}
            showCreatedAt={(passkeyNameCounts.get(identity.name) ?? 0) > 1}
          />
          <div class="ms-auto flex shrink-0 items-center gap-2">
            {#if identity.identityNumber === selected}
              <span
                class="bg-bg-success-primary border-bg-success-secondary inline-flex shrink-0 items-center gap-1 rounded-full border py-0.5 ps-1.5 pe-2"
              >
                <span class="bg-fg-success-primary size-1.5 rounded-full"
                ></span>
                <span class="text-text-success-primary text-xs font-medium">
                  {$t`Signed in`}
                </span>
              </span>
            {/if}
            <Tooltip
              label={$t`Sign out to remove`}
              direction="left"
              hidden={identity.identityNumber !== selected}
              distance="0.25rem"
            >
              <button
                onclick={() => (removingIdentity = identity)}
                disabled={identity.identityNumber === selected}
                class="btn btn-tertiary btn-icon size-8! shrink-0 rounded-full"
              >
                <Trash2Icon class="size-5" />
                <span>{$t`Remove`}</span>
              </button>
            </Tooltip>
          </div>
        </li>
        {#if i < identities.length - 1}
          <div class="border-border-secondary border-t"></div>
        {/if}
      {/each}
    </ul>
  </div>
{/snippet}

{#if removingIdentity !== undefined}
  {@render removeConfirmation(removingIdentity)}
{:else}
  {@render identityList()}
{/if}
