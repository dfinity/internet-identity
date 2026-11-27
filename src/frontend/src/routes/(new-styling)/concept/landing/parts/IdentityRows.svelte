<!--
  The identities on this device as rows, the way the concept's identity
  list draws them (`features/identity/IdentityList.svelte`): the avatar, the
  name, what it signs in with, rows parted by inset hairlines. Then Add
  identity. The one used last carries "Last used" where there's more than
  one to tell apart; editing puts a remove button on each row.
-->
<script lang="ts">
  import { ChevronRightIcon, MinusIcon, PlusIcon } from "@lucide/svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { fade, slide } from "svelte/transition";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { methodOf, nameOf } from "../mock";
  import type { Playground } from "../playground.svelte";
  import LastUsed from "./LastUsed.svelte";

  interface Props {
    p: Playground;
    /** Leaves some out, e.g. the one a layout already shows larger. */
    identities?: LastUsedIdentity[];
    withAdd?: boolean;
    /** `lg`: 48px avatars and a larger name, for a list that is the page. */
    size?: "md" | "lg";
  }

  const { p, identities, withAdd = true, size = "md" }: Props = $props();

  const list = $derived(identities ?? p.identities);
  const large = $derived(size === "lg");

  const ROW =
    "flex w-full items-center text-start outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-focus-ring";
</script>

<ul
  aria-label="Identities on this device"
  class={[
    "ii-rows flex flex-col",
    large ? "[--ii-rows-inset:5rem]" : "[--ii-rows-inset:3.75rem]",
  ]}
>
  {#each list as one (one.identityNumber)}
    {@const pending = p.pending === one.identityNumber}
    {@const recent =
      p.lastUsed &&
      p.identities.length > 1 &&
      p.recent?.identityNumber === one.identityNumber}
    <li
      class="flex items-center"
      out:slide={{
        duration: prefersReducedMotion.current ? 0 : 200,
        easing: quintOut,
      }}
    >
      <button
        type="button"
        onclick={() => p.signIn(one)}
        onpointerenter={() => (p.hovered = one.identityNumber)}
        onpointerleave={() => (p.hovered = undefined)}
        onfocus={() => (p.hovered = one.identityNumber)}
        onblur={() => (p.hovered = undefined)}
        disabled={p.editing}
        aria-busy={pending}
        class={[
          "group ii-hover",
          ROW,
          large ? "gap-4 px-4 py-3.5" : "gap-3 px-4 py-3",
          p.editing && "cursor-default",
        ]}
      >
        {#if large}
          <IdentityAvatar identity={one} size="lg" />
        {:else}
          <IdentityAvatar identity={one} size="sm" />
        {/if}
        <span class="flex min-w-0 flex-1 flex-col">
          <span
            class={[
              "text-text-primary truncate font-medium",
              large ? "text-base" : "text-sm",
            ]}
          >
            {nameOf(one)}
          </span>
          <span
            class={[
              "text-text-tertiary truncate",
              large ? "text-sm" : "text-[13px] leading-[18px]",
            ]}
          >
            {methodOf(one)}
          </span>
        </span>
        {#if recent}
          <LastUsed />
        {/if}
        {#if pending}
          <ProgressRing class="text-fg-tertiary size-4 shrink-0" />
        {:else if !p.editing}
          <ChevronRightIcon
            aria-hidden="true"
            class="text-fg-quaternary size-4 shrink-0 -translate-x-1 opacity-0 transition-[translate,opacity] duration-200 ease-(--ii-ease-out) group-hover:translate-x-0 group-hover:opacity-100 group-focus-visible:translate-x-0 group-focus-visible:opacity-100 motion-reduce:transition-none rtl:-scale-x-100"
          />
        {/if}
      </button>
      {#if p.editing}
        <button
          type="button"
          onclick={() => p.remove(one)}
          aria-label={`Remove ${nameOf(one)} from this device`}
          title="Remove from this device"
          class="text-fg-error-primary focus-visible:ring-focus-ring me-3 grid size-8 shrink-0 place-items-center rounded-full bg-(--ii-error-tint) outline-none focus-visible:ring-2"
          transition:fade={{ duration: 150 }}
        >
          <MinusIcon class="size-4" strokeWidth={2.5} />
        </button>
      {/if}
    </li>
  {/each}
  {#if withAdd}
    <li>
      <button
        type="button"
        onclick={() => (p.methodsOpen = true)}
        class={[
          "ii-hover",
          ROW,
          large ? "gap-4 px-4 py-3.5" : "gap-3 px-4 py-3",
        ]}
      >
        <span
          aria-hidden="true"
          class={[
            "border-border-secondary text-fg-tertiary grid shrink-0 place-items-center rounded-full border border-dashed",
            large ? "size-12" : "size-8",
          ]}
        >
          <PlusIcon class={large ? "size-5" : "size-4"} />
        </span>
        <span
          class={[
            "text-text-primary font-medium",
            large ? "text-base" : "text-sm",
          ]}
        >
          Add identity
        </span>
      </button>
    </li>
  {/if}
</ul>
