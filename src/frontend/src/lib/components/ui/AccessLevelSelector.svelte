<script lang="ts">
  import type { ClassValue } from "svelte/elements";
  import Radiobox from "$lib/components/ui/Radiobox.svelte";
  import { t } from "$lib/stores/locale.store";
  import type { AccessLevel } from "$lib/utils/accessLevel";

  interface Props {
    /** The chosen access level (bindable), or `undefined` when nothing is
     *  selected yet. `undefined` is the first-time state: the two options are
     *  shown unselected so the user makes an explicit choice, rather than
     *  silently inheriting a default nobody notices. "read-only" restricts the
     *  session delegation to query calls (the app can read on the user's behalf
     *  but cannot change state — the Internet Computer rejects update calls
     *  authenticated through it); "full-access" is update-capable. */
    accessLevel: AccessLevel | undefined;
    disabled?: boolean;
    /** Extra classes for the wrapper (e.g. margins), so each flow can control
     *  spacing without the component hard-coding it. */
    class?: ClassValue;
  }

  let {
    accessLevel = $bindable(),
    disabled = false,
    class: className,
  }: Props = $props();
</script>

<fieldset class={["flex flex-col", className]} {disabled}>
  <legend class="text-text-secondary mb-3 text-sm font-medium">
    {$t`Permissions`}
  </legend>
  <div class="flex flex-col gap-3">
    <Radiobox
      bind:group={accessLevel}
      value="read-only"
      name="access-level"
      size="sm"
      label={$t`Questions only`}
      hint={$t`Looks things up, changing nothing.`}
      {disabled}
    />
    <Radiobox
      bind:group={accessLevel}
      value="full-access"
      name="access-level"
      size="sm"
      label={$t`Actions & questions`}
      hint={$t`Also makes changes on your behalf.`}
      {disabled}
    />
  </div>
</fieldset>
