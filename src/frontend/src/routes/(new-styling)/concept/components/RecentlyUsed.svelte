<script lang="ts" module>
  export interface RecentEntry {
    origin: string;
    name: string;
    logo: string;
    url: string;
    usedMillis: number;
    /** In My apps already; otherwise the card offers to add it. */
    added: boolean;
  }
</script>

<script lang="ts">
  import { ChevronDownIcon, ClockIcon, PlusIcon } from "@lucide/svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { slide } from "svelte/transition";
  import { formatRelative } from "$lib/stores/locale.store";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";

  interface Props {
    entries: RecentEntry[];
    onAdd: (origin: string) => void;
    onOpen: (origin: string) => void;
  }

  const { entries, onAdd, onOpen }: Props = $props();

  let open = $state(false);
  const notAdded = $derived(entries.filter(({ added }) => !added).length);
  const listId = $props.id();
</script>

<!-- History, like a video site's: out of the way under the grid, a grey row
     that opens into a strip of what was used lately, newest first. Apps used
     but never added to the grid can be added from here. Removed apps don't
     show: removing one means not seeing it again. -->
{#if entries.length > 0}
  <section class="mt-8 w-full rounded-2xl bg-(--ii-hover)">
    <button
      type="button"
      onclick={() => (open = !open)}
      aria-expanded={open}
      aria-controls={listId}
      class="text-text-tertiary hover:text-text-secondary focus-visible:ring-focus-ring flex w-full items-center gap-2 rounded-2xl px-4 py-2.5 text-start text-[13px] leading-[18px] font-medium transition-colors duration-150 outline-none focus-visible:ring-2"
    >
      <ClockIcon class="size-3.5 shrink-0" aria-hidden="true" />
      <span>Recently used</span>
      <span class="text-text-quaternary font-normal tabular-nums">
        · {entries.length}{notAdded > 0 ? `, ${notAdded} not in My apps` : ""}
      </span>
      <ChevronDownIcon
        aria-hidden="true"
        class={[
          "ms-auto size-4 shrink-0 transition-transform duration-200 ease-(--ii-ease-out) motion-reduce:transition-none",
          open && "rotate-180",
        ]}
      />
    </button>
    {#if open}
      <div
        transition:slide={{
          duration: prefersReducedMotion.current ? 0 : 240,
          easing: quintOut,
        }}
      >
        <ul
          id={listId}
          class="flex [scrollbar-width:none] gap-2 overflow-x-auto overscroll-x-contain px-3 pb-3 [&::-webkit-scrollbar]:hidden"
        >
          {#each entries as entry (entry.origin)}
            <li
              class="bg-bg-primary border-border-tertiary flex w-48 shrink-0 items-center gap-2.5 rounded-xl border py-2 ps-2 pe-2.5"
            >
              <a
                href={entry.url}
                target="_blank"
                rel="noopener noreferrer"
                onclick={() => onOpen(entry.origin)}
                class="focus-visible:ring-focus-ring flex min-w-0 flex-1 items-center gap-2.5 rounded-lg outline-none focus-visible:ring-2"
              >
                <AppLogo logo={entry.logo} size="card" />
                <span class="flex min-w-0 flex-col">
                  <span class="text-text-primary truncate text-sm font-medium">
                    {entry.name}
                  </span>
                  <span class="text-text-tertiary truncate text-xs">
                    {$formatRelative(new Date(entry.usedMillis), {
                      style: "short",
                    })}
                  </span>
                </span>
              </a>
              {#if !entry.added}
                <button
                  type="button"
                  onclick={() => onAdd(entry.origin)}
                  aria-label={`Add ${entry.name} to My apps`}
                  title="Add to My apps"
                  class="border-border-secondary text-fg-secondary hover:bg-bg-primary_hover focus-visible:ring-focus-ring grid size-7 shrink-0 place-items-center rounded-full border transition-colors duration-150 outline-none focus-visible:ring-2"
                >
                  <PlusIcon class="size-3.5" />
                </button>
              {/if}
            </li>
          {/each}
        </ul>
      </div>
    {/if}
  </section>
{/if}
