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
  } from "@lucide/svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { slide } from "svelte/transition";
  import IdentityAvatar from "$lib/components/ui/IdentityAvatar.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { getMetadataString } from "$lib/utils/openID";
  import { swapIn, swapOut } from "../motion";
  import Elevated from "./Elevated.svelte";

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

  // The clipboard refuses in some embedded or insecure contexts; then the row
  // simply doesn't say "copied".
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

  const ROW =
    "ii-hover focus-visible:ring-focus-ring flex w-full items-center text-start outline-none focus-visible:ring-2 focus-visible:ring-inset";
</script>

<!-- What the account in the corner leaves out: it shows a name and an avatar,
     and never changes size. So the identity's number has a row here, a label
     and its value as in a settings list, and so does switching identity. The
     other identities open in the card itself, under their row, and push the
     panel down: nothing floats over the panel the account already opened. -->
<Elevated
  offset={1}
  as="ul"
  class="ii-rows flex flex-col overflow-hidden rounded-2xl"
>
  <li>
    <button
      type="button"
      onclick={copyNumber}
      aria-label={`Identity number ${identity.identityNumber}. Copy`}
      class={[ROW, "gap-2 px-4 py-3"]}
    >
      <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
        Identity number
      </span>
      <span class="text-text-tertiary text-sm tabular-nums">
        {identity.identityNumber}
      </span>
      <span
        aria-hidden="true"
        class="text-fg-quaternary grid size-4 shrink-0 place-items-center *:col-start-1 *:row-start-1"
      >
        {#if copied}
          <span
            class="text-fg-success-primary"
            in:swapIn={{ reduced: prefersReducedMotion.current }}
            out:swapOut={{ reduced: prefersReducedMotion.current }}
          >
            <CheckIcon class="size-4" strokeWidth={2.5} />
          </span>
        {:else}
          <span
            in:swapIn={{ reduced: prefersReducedMotion.current }}
            out:swapOut={{ reduced: prefersReducedMotion.current }}
          >
            <CopyIcon class="size-4" />
          </span>
        {/if}
      </span>
    </button>
    <span class="sr-only" aria-live="polite">
      {copied ? "Identity number copied" : ""}
    </span>
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
      <!-- Closed, the row names who there is to switch to, where the row
           above has its value; open, the list below says it. -->
      <button
        type="button"
        onclick={() => (switching = !switching)}
        aria-expanded={switching}
        aria-controls={SWITCH_LIST_ID}
        class={[ROW, "gap-2 px-4 py-3"]}
      >
        <span class="text-text-primary shrink-0 text-sm font-medium">
          Switch identity
        </span>
        <span
          aria-hidden="true"
          class={[
            "text-text-tertiary min-w-0 flex-1 truncate text-end text-sm transition-opacity duration-150 ease-out motion-reduce:transition-none",
            switching && "opacity-0",
          ]}
        >
          {others.map(nameOf).join(", ")}
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
