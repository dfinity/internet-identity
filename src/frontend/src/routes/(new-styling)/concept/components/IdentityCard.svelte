<script lang="ts" module>
  /** The list of identities to switch to, for whatever else opens it. */
  export const SWITCH_LIST_ID = "identity-switch-list";
</script>

<script lang="ts">
  import {
    CheckIcon,
    ChevronDownIcon,
    CopyIcon,
    PlusIcon,
    UserIcon,
  } from "@lucide/svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { slide } from "svelte/transition";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { identityInitial, identityPalette } from "$lib/utils/identityColors";
  import { getMetadataString } from "$lib/utils/openID";
  import { swapIn, swapOut } from "../motion";
  import Elevated, { useSurface } from "./Elevated.svelte";

  interface Props {
    identity: LastUsedIdentity;
    /** The device's other identities, to switch to. */
    others: LastUsedIdentity[];
    /** Whether the other identities are listed. */
    switching?: boolean;
    onSwitch: (identityNumber: bigint) => void;
    onAdd: () => void;
  }

  let {
    identity,
    others,
    switching = $bindable(false),
    onSwitch,
    onAdd,
  }: Props = $props();

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

  // The clipboard refuses in some embedded or insecure contexts; then the
  // number simply doesn't say "copied".
  let copied = $state(false);
  let copiedTimer: ReturnType<typeof setTimeout> | undefined;
  const copyNumber = async () => {
    try {
      await navigator.clipboard.writeText(`${identity.identityNumber}`);
    } catch {
      return;
    }
    copied = true;
    clearTimeout(copiedTimer);
    copiedTimer = setTimeout(() => (copied = false), 1800);
  };

  /** The row's avatars show the first few; the rest are a count. */
  const PREVIEW = 3;
  // The card is one step above what it stands on (Elevated, below), and the
  // avatars in the preview cut themselves out of it.
  const cardSurface = `var(--ii-surface-${Math.min(useSurface() + 1, 8)})`;

  const ROW =
    "ii-hover focus-visible:ring-focus-ring flex w-full items-center text-start outline-none focus-visible:ring-2 focus-visible:ring-inset";
</script>

<!-- Whose panel this is, as an account switcher heads its menu (Google's
     accounts, a workspace or an organization): the identity itself, large,
     with its name and number, and under it the way to another one. The other
     identities show as a row of avatars, so the row says who there is at a
     glance; they open in the card itself and push the panel down, so nothing
     floats over the panel. -->
<Elevated
  offset={1}
  as="ul"
  class="ii-rows flex flex-col overflow-hidden rounded-2xl"
>
  <li class="flex items-center gap-3 px-4 pt-4 pb-3.5">
    <IdentityAvatar {identity} size="lg" />
    <div class="flex min-w-0 flex-1 flex-col items-start">
      <span
        class="text-text-primary w-full truncate text-base leading-6 font-semibold"
      >
        {nameOf(identity)}
      </span>
      <!-- The number copies from where it is said, with the glyph after it
           that says so. -->
      <button
        type="button"
        onclick={copyNumber}
        aria-label={`Identity number ${identity.identityNumber}. Copy`}
        class="ii-hover focus-visible:ring-focus-ring text-text-tertiary hover:text-text-secondary -mx-1.5 flex items-center gap-1.5 rounded-md px-1.5 text-sm leading-6 transition-colors duration-150 outline-none focus-visible:ring-2"
      >
        <span>
          Identity number <span class="tabular-nums"
            >{identity.identityNumber}</span
          >
        </span>
        <span
          aria-hidden="true"
          class="grid size-3.5 shrink-0 place-items-center *:col-start-1 *:row-start-1"
        >
          {#if copied}
            <span
              class="text-fg-success-primary"
              in:swapIn={{ reduced: prefersReducedMotion.current }}
              out:swapOut={{ reduced: prefersReducedMotion.current }}
            >
              <CheckIcon class="size-3.5" strokeWidth={2.5} />
            </span>
          {:else}
            <span
              class="text-fg-quaternary"
              in:swapIn={{ reduced: prefersReducedMotion.current }}
              out:swapOut={{ reduced: prefersReducedMotion.current }}
            >
              <CopyIcon class="size-3.5" />
            </span>
          {/if}
        </span>
      </button>
      <span class="sr-only" aria-live="polite">
        {copied ? "Identity number copied" : ""}
      </span>
    </div>
  </li>
  {#if others.length === 0}
    <li>
      <button type="button" onclick={onAdd} class={[ROW, "gap-2 px-4 py-3"]}>
        <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
          Add identity
        </span>
        <PlusIcon
          class="text-fg-quaternary size-4 shrink-0"
          aria-hidden="true"
        />
      </button>
    </li>
  {:else}
    <li>
      <!-- Closed, the row shows who there is to switch to, as overlapping
           avatars in their own colours; open, the list below says it. -->
      <button
        type="button"
        onclick={() => (switching = !switching)}
        aria-expanded={switching}
        aria-controls={SWITCH_LIST_ID}
        class={[ROW, "gap-2 px-4 py-3"]}
      >
        <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
          Switch identity
        </span>
        <span
          aria-hidden="true"
          class={[
            "flex shrink-0 items-center transition-opacity duration-150 ease-out motion-reduce:transition-none",
            switching && "opacity-0",
          ]}
          style:--ii-stack-cut={cardSurface}
        >
          {#each others.slice(0, PREVIEW) as other, index (other.identityNumber)}
            {@const palette = identityPalette(other.identityNumber)}
            {@const initial = identityInitial(other.name)}
            <!-- Later avatars tuck under earlier ones, each cut out of the
                 card by a ring, so the first reads as the front. -->
            <span
              class={[
                "relative grid size-6 place-items-center rounded-full text-[11px] leading-none font-semibold ring-2 ring-(color:--ii-stack-cut) outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12",
                index > 0 && "-ms-1.5",
              ]}
              style:background-color={palette.background}
              style:color={palette.foreground}
              style:z-index={PREVIEW - index}
            >
              {#if initial !== undefined}
                {initial}
              {:else}
                <UserIcon class="size-3.5" />
              {/if}
            </span>
          {/each}
          {#if others.length > PREVIEW}
            <span
              class="bg-bg-secondary text-text-tertiary relative -ms-1.5 grid h-6 min-w-6 place-items-center rounded-full px-1 text-[11px] leading-none font-medium tabular-nums ring-2 ring-(color:--ii-stack-cut)"
            >
              +{others.length - PREVIEW}
            </span>
          {/if}
        </span>
        <ChevronDownIcon
          aria-hidden="true"
          class={[
            "text-fg-quaternary size-4 shrink-0 transition-transform duration-200 ease-(--ii-ease-out) motion-reduce:transition-none",
            switching && "rotate-180",
          ]}
        />
      </button>
    </li>
    {#if switching}
      <li
        transition:slide={{
          duration: prefersReducedMotion.current ? 0 : 240,
          easing: quintOut,
        }}
      >
        <!-- An avatar is 32px, so a line here starts 60px in. -->
        <ul
          id={SWITCH_LIST_ID}
          aria-label="Switch identity"
          class="ii-rows flex flex-col [--ii-rows-inset:3.75rem]"
        >
          {#each others as other (other.identityNumber)}
            <li>
              <button
                type="button"
                onclick={() => onSwitch(other.identityNumber)}
                class={[ROW, "gap-3 px-4 py-2.5"]}
              >
                <IdentityAvatar identity={other} size="sm" />
                <span class="flex min-w-0 flex-1 flex-col">
                  <span class="text-text-primary truncate text-sm font-medium">
                    {nameOf(other)}
                  </span>
                  <span
                    class="text-text-tertiary truncate text-[13px] leading-[18px]"
                  >
                    {methodOf(other)}
                  </span>
                </span>
              </button>
            </li>
          {/each}
          <li>
            <button
              type="button"
              onclick={onAdd}
              class={[ROW, "gap-3 px-4 py-2.5"]}
            >
              <span
                aria-hidden="true"
                class="border-border-secondary text-fg-tertiary grid size-8 shrink-0 place-items-center rounded-full border border-dashed"
              >
                <PlusIcon class="size-4" />
              </span>
              <span class="text-text-primary text-sm font-medium">
                Add identity
              </span>
            </button>
          </li>
        </ul>
      </li>
    {/if}
  {/if}
</Elevated>
