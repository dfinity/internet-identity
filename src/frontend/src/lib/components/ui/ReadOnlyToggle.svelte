<script lang="ts">
  import { HelpCircleIcon } from "@lucide/svelte";
  import type { ClassValue } from "svelte/elements";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Whether read-only mode is enabled (bindable). When `true`, the session
     *  delegation is restricted to query calls: the app can read on the user's
     *  behalf but cannot change state. Enforcement is protocol-level (the
     *  Internet Computer rejects update calls authenticated through it). */
    checked: boolean;
    disabled?: boolean;
    /** Extra classes for the row wrapper (e.g. top margin), so each flow can
     *  control spacing without the component hard-coding it. */
    class?: ClassValue;
  }

  let {
    checked = $bindable(),
    disabled = false,
    class: className,
  }: Props = $props();
</script>

<div class={["flex flex-row items-center", className]}>
  <Checkbox
    bind:checked
    label={$t`Read-only mode`}
    size="sm"
    {disabled}
  />
  <Tooltip
    label={$t`Read-only mode`}
    description={$t`When enabled, the app can read your data but cannot make changes on your behalf: the Internet Computer rejects any call that would change state.`}
    direction="up"
    align="end"
    offset="0rem"
    class="max-w-80"
  >
    <button
      class="btn btn-tertiary btn-sm btn-icon ms-auto !cursor-default !rounded-full"
      aria-label={$t`More information about read-only mode`}
    >
      <HelpCircleIcon class="size-5" />
    </button>
  </Tooltip>
</div>
