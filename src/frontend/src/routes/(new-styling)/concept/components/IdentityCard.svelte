<script lang="ts">
  import { CheckIcon, CopyIcon } from "@lucide/svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { swapIn, swapOut } from "../motion";
  import Elevated from "./Elevated.svelte";

  interface Props {
    identity: LastUsedIdentity;
  }

  const { identity }: Props = $props();

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
</script>

<!-- What the account in the corner leaves out: it shows a name and an avatar,
     and never changes size, so the identity's number has a row of its own
     here. A label and its value, as in a settings list; the row copies it. -->
<Elevated offset={1} as="ul" class="flex flex-col overflow-hidden rounded-2xl">
  <li>
    <button
      type="button"
      onclick={copyNumber}
      aria-label={`Identity number ${identity.identityNumber}. Copy`}
      class="ii-hover focus-visible:ring-focus-ring flex w-full items-center gap-2 px-4 py-3 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset"
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
</Elevated>
