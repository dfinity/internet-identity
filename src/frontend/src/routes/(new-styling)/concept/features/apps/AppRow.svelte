<script lang="ts">
  /**
   * An app as a row, the way the App Store lists one: icon, name, one line
   * about it, and one action at the end. Add while the app isn't in My apps,
   * Open once it is, at the same width so the row doesn't shift when it
   * turns. The rest of the row opens the app.
   */
  import AppLogo from "../../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { FeaturedApp } from "../../mock/mockData";

  interface Props {
    /** A pasted link has no logo, and shows the globe. */
    app: Omit<FeaturedApp, "logo"> & { logo?: string };
    added: boolean;
    onAdd: () => void;
  }

  const { app, added, onAdd }: Props = $props();

  // Positioned, so it sits above the row's link, which covers the whole row.
  const ACTION =
    "btn btn-sm relative min-w-16 shrink-0 rounded-full font-medium transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
</script>

<div class="ii-hover relative flex items-center gap-3 rounded-2xl p-2">
  <a
    href={app.url}
    target="_blank"
    rel="noopener noreferrer"
    class="focus-visible:after:outline-focus-ring flex min-w-0 flex-1 items-center gap-3 outline-none after:absolute after:inset-0 after:rounded-2xl focus-visible:after:outline-2"
  >
    <AppLogo logo={app.logo} size="lg" />
    <span class="flex min-w-0 flex-col gap-0.5">
      <span class="text-text-primary truncate text-sm font-semibold">
        {app.name}
      </span>
      {#if app.oneLiner !== undefined}
        <span class="text-text-tertiary truncate text-xs">
          {app.oneLiner}
        </span>
      {/if}
    </span>
  </a>
  {#if added}
    <a
      href={app.url}
      target="_blank"
      rel="noopener noreferrer"
      aria-label={`Open ${app.name}`}
      class={[ACTION, "btn-secondary"]}
    >
      Open
    </a>
  {:else}
    <button
      type="button"
      onclick={onAdd}
      aria-label={`Add ${app.name}`}
      class={[ACTION, "btn-primary"]}
    >
      Add
    </button>
  {/if}
</div>
