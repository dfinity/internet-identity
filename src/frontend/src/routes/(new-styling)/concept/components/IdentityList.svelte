<script lang="ts">
  import { CheckIcon, PlusIcon } from "@lucide/svelte";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import type { IdentityPalette } from "$lib/utils/identityColors";
  import { getMetadataString } from "$lib/utils/openID";
  import { still, type Travel } from "../motion";

  interface Props {
    id: string;
    identity: LastUsedIdentity;
    /** What this browser signed in with, for the current identity's row. */
    signedInWith: string;
    others: LastUsedIdentity[];
    onSwitch: (identityNumber: bigint) => void;
    onAdd: () => void;
    paletteOf?: (one: LastUsedIdentity) => IdentityPalette | undefined;
    /** Lists the current identity first, checked. Off where the current
     *  identity already heads the list (the panel's card). */
    withCurrent?: boolean;
    /** `md`: 36px avatars, the panel cards' icon size, so the names start on
     *  their 64px line. */
    size?: "sm" | "md";
    /** The crossfade pair the stack shares: each avatar arrives from it. */
    travel?: Travel;
  }

  const {
    id,
    identity,
    signedInWith,
    others,
    onSwitch,
    onAdd,
    paletteOf,
    withCurrent = true,
    size = "sm",
    travel,
  }: Props = $props();

  // svelte-ignore state_referenced_locally
  const [travelOut, travelIn] = travel ?? [still, still];
  /** The product's avatar comes at 32px; the panel's cards stand at 36. */
  const AVATAR = $derived(
    size === "md" ? "shrink-0 [&>div>div]:size-9" : "shrink-0",
  );

  const nameOf = (one: LastUsedIdentity): string =>
    one.name ?? `${one.identityNumber}`;

  /** What an identity signs in with, as the product's switcher says it. */
  const methodOf = ({ authMethod }: LastUsedIdentity): string => {
    if ("openid" in authMethod && authMethod.openid.metadata !== undefined) {
      return (
        getMetadataString(authMethod.openid.metadata, "email") ?? "Hidden email"
      );
    }
    if ("sso" in authMethod) {
      const { sso } = authMethod;
      return sso.email ?? sso.name ?? sso.domain;
    }
    return "Passkey";
  };

  const ROW =
    "flex w-full items-center gap-3 px-4 py-2.5 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-focus-ring";
</script>

<!-- Every identity on this device, the current one first and checked, as
     X lists the accounts it can switch between; then the way to add one.
     An avatar is 32px, so a line here starts 60px in; at `md`, 36 and 64. -->
<ul
  {id}
  aria-label="Identities"
  class={[
    "ii-rows flex flex-col",
    size === "md" ? "[--ii-rows-inset:4rem]" : "[--ii-rows-inset:3.75rem]",
  ]}
>
  {#if withCurrent}
    <li>
      <div class={ROW} aria-current="true">
        <IdentityAvatar {identity} palette={paletteOf?.(identity)} size="sm" />
        <span class="flex min-w-0 flex-1 flex-col">
          <span class="text-text-primary truncate text-sm font-medium">
            {nameOf(identity)}
          </span>
          <span class="text-text-tertiary truncate text-[13px] leading-[18px]">
            {signedInWith}
          </span>
        </span>
        <CheckIcon
          class="text-fg-primary size-4 shrink-0"
          strokeWidth={2.5}
          aria-label="Current"
        />
      </div>
    </li>
  {/if}
  {#each others as other (other.identityNumber)}
    <li>
      <button
        type="button"
        onclick={() => onSwitch(other.identityNumber)}
        class={["ii-hover", ROW]}
      >
        <span
          class={AVATAR}
          in:travelIn|global={{ key: other.identityNumber }}
          out:travelOut|global={{ key: other.identityNumber }}
        >
          <IdentityAvatar
            identity={other}
            palette={paletteOf?.(other)}
            size="sm"
          />
        </span>
        <span class="flex min-w-0 flex-1 flex-col">
          <span class="text-text-primary truncate text-sm font-medium">
            {nameOf(other)}
          </span>
          <span class="text-text-tertiary truncate text-[13px] leading-[18px]">
            {methodOf(other)}
          </span>
        </span>
      </button>
    </li>
  {/each}
  <li>
    <button type="button" onclick={onAdd} class={["ii-hover", ROW]}>
      <span
        aria-hidden="true"
        class={[
          "border-border-secondary text-fg-tertiary grid shrink-0 place-items-center rounded-full border border-dashed",
          size === "md" ? "size-9" : "size-8",
        ]}
      >
        <PlusIcon class="size-4" />
      </span>
      <span class="text-text-primary text-sm font-medium">Add identity</span>
    </button>
  </li>
</ul>
