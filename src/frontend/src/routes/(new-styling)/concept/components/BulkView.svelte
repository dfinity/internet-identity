<script lang="ts">
  import { BellIcon } from "@lucide/svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { ConceptApp } from "../mockData";
  import Elevated, { useSurface } from "./Elevated.svelte";
  import PanelHeader from "./PanelHeader.svelte";

  interface Props {
    apps: ConceptApp[];
    onExit: () => void;
    onSetNotifications: (allowed: boolean) => void;
    onRemove: () => void;
  }

  const { apps, onExit, onSetNotifications, onRemove }: Props = $props();

  const notifying = $derived(
    apps.filter(({ notificationsAllowed }) => notificationsAllowed).length,
  );
  const switchId = $props.id();

  // The logos overlap, each cut out of the section they sit in, two steps
  // above wherever the view is (the panel, or the phone's sheet).
  const sectionSurface = `var(--ii-surface-${Math.min(useSurface() + 2, 8)})`;
</script>

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
    style={`--ii-card-surface: ${sectionSurface}`}
  >
    <div class="flex" aria-hidden="true">
      {#each apps.slice(0, 5) as app (app.origin)}
        <span class="-me-3 rounded-lg ring-3 ring-(color:--ii-card-surface)">
          <AppLogo logo={app.logo} size="card" />
        </span>
      {/each}
    </div>
    <div class="flex min-w-0 flex-col">
      <h3 class="text-text-primary text-sm font-medium">
        {apps.length} apps selected
      </h3>
      <span class="text-text-tertiary truncate text-[13px] leading-[18px]">
        {apps.map(({ name }) => name).join(", ")}
      </span>
    </div>
  </Elevated>

  <Elevated offset={1} class="flex items-center gap-3 rounded-2xl px-4 py-3.5">
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
