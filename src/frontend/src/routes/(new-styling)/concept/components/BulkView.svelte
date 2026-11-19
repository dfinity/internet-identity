<script lang="ts">
  import { BellIcon, Grid2x2CheckIcon } from "@lucide/svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { ConceptApp } from "../mockData";
  import Elevated, { useSurface } from "./Elevated.svelte";
  import PanelCard from "./PanelCard.svelte";
  import PanelHeader from "./PanelHeader.svelte";

  interface Props {
    apps: ConceptApp[];
    /** `page`: a view of its own, as in a phone's sheet, with Done over it.
     *  `card`: one card with Close, as an open section is beside the canvas. */
    variant?: "page" | "card";
    onExit: () => void;
    onSetNotifications: (allowed: boolean) => void;
    onRemove: () => void;
  }

  const {
    apps,
    variant = "page",
    onExit,
    onSetNotifications,
    onRemove,
  }: Props = $props();

  const notifying = $derived(
    apps.filter(({ notificationsAllowed }) => notificationsAllowed).length,
  );
  const switchId = $props.id();
  const names = $derived(apps.map(({ name }) => name).join(", "));

  // The logos overlap, each cut out of what they sit on: two steps above
  // wherever the page view is (the panel, or the phone's sheet), and on the
  // card, the card itself, one step up.
  // svelte-ignore state_referenced_locally
  const logoSurface = `var(--ii-surface-${Math.min(useSurface() + (variant === "card" ? 1 : 2), 8)})`;
</script>

{#snippet logos()}
  <div class="flex" aria-hidden="true" style:--ii-card-surface={logoSurface}>
    {#each apps.slice(0, 5) as app (app.origin)}
      <span class="-me-3 rounded-lg ring-3 ring-(color:--ii-card-surface)">
        <AppLogo logo={app.logo} size="card" />
      </span>
    {/each}
  </div>
{/snippet}

{#snippet notificationsRow()}
  <span
    aria-hidden="true"
    class="text-fg-secondary flex size-9 shrink-0 items-center justify-center rounded-lg bg-(--ii-selected)"
  >
    <BellIcon class="size-4.5" />
  </span>
  <span class="flex min-w-0 flex-1 flex-col gap-0.5">
    <span id={switchId} class="text-text-primary text-sm font-medium">
      Notifications
    </span>
    <span class="text-text-tertiary text-[13px] leading-[18px]">
      {#if notifying === apps.length}
        On for all of them
      {:else if notifying === 0}
        Off for all of them
      {:else}
        On for {notifying} of {apps.length}
      {/if}
    </span>
  </span>
  <Toggle
    checked={notifying === apps.length}
    onchange={(event) => onSetNotifications(event.currentTarget.checked)}
    aria-labelledby={switchId}
  />
{/snippet}

{#if variant === "card"}
  <!-- The same card an app's own options open in, for all the picked ones
       at once: what's picked, the setting they share, removing them. -->
  <PanelCard
    title={`${apps.length} apps selected`}
    closeLabel="Done"
    onClose={onExit}
  >
    {#snippet icon(large: boolean)}
      <span
        aria-hidden="true"
        class={[
          "text-fg-secondary grid shrink-0 place-items-center bg-(--ii-selected)",
          large ? "size-11 rounded-xl" : "size-9 rounded-[0.625rem]",
        ]}
      >
        <Grid2x2CheckIcon class={large ? "size-5.5" : "size-4.5"} />
      </span>
    {/snippet}
    {#snippet lead()}
      <span class="line-clamp-2">{names}</span>
    {/snippet}
    <div class="px-4 pt-4">{@render logos()}</div>
    <Elevated
      offset={1}
      shadowLevel={3}
      class="mx-2 mt-4 flex items-center gap-3 overflow-hidden rounded-lg px-2 py-3.5"
    >
      {@render notificationsRow()}
    </Elevated>
    <div class="flex flex-col items-start px-4 pt-2 pb-3">
      <button
        type="button"
        onclick={onRemove}
        class="btn btn-tertiary btn-danger btn-sm -ms-3 font-medium"
      >
        Remove {apps.length} apps
      </button>
      <span class="text-text-tertiary text-[13px] leading-[18px]">
        Signs you out of them and clears their settings.
      </span>
    </div>
  </PanelCard>
{:else}
  <div class="flex flex-col gap-4">
    <PanelHeader
      title="Selected apps"
      lead="Change them all at once."
      backLabel="Done"
      onBack={onExit}
    />

    <Elevated
      offset={2}
      as="section"
      class="flex flex-col gap-4 rounded-2xl p-4"
    >
      {@render logos()}
      <div class="flex min-w-0 flex-col">
        <h3 class="text-text-primary text-sm font-medium">
          {apps.length} apps selected
        </h3>
        <span class="text-text-tertiary truncate text-[13px] leading-[18px]">
          {names}
        </span>
      </div>
    </Elevated>

    <Elevated
      offset={1}
      class="flex items-center gap-3 rounded-2xl px-4 py-3.5"
    >
      {@render notificationsRow()}
    </Elevated>

    <div class="flex flex-col items-center gap-1 pt-2">
      <button
        type="button"
        onclick={onRemove}
        class="btn btn-tertiary btn-danger btn-sm font-medium"
      >
        Remove {apps.length} apps
      </button>
      <span class="text-text-tertiary text-center text-[13px] leading-[18px]">
        Signs you out of them and clears their settings.
      </span>
    </div>
  </div>
{/if}
