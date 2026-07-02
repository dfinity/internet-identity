<script lang="ts">
  import { HelpCircleIcon } from "@lucide/svelte";
  import type { ClassValue } from "svelte/elements";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { t } from "$lib/stores/locale.store";
  import type { AccessLevel } from "$lib/utils/accessLevel";

  interface Props {
    /** The chosen access level (bindable). "read-only" restricts the session
     *  delegation to query calls: the app can read on the user's behalf but
     *  cannot change state. Enforcement is protocol-level (the Internet
     *  Computer rejects update calls authenticated through it). */
    accessLevel: AccessLevel;
    /** Which opt-in the checkbox offers — ticking the box always *changes*
     *  the flow's default. Flows that default to full access show a
     *  "Read-only mode" checkbox (`"read-only"`); flows that default to
     *  read-only show a "Full access" checkbox (`"full-access"`). */
    prompt: AccessLevel;
    disabled?: boolean;
    /** Extra classes for the row wrapper (e.g. top margin), so each flow can
     *  control spacing without the component hard-coding it. */
    class?: ClassValue;
  }

  let {
    accessLevel = $bindable(),
    prompt,
    disabled = false,
    class: className,
  }: Props = $props();

  const label = $derived(
    prompt === "read-only" ? $t`Read-only mode` : $t`Full access`,
  );
  const description = $derived(
    prompt === "read-only"
      ? $t`When enabled, the app can read your data but cannot make changes on your behalf: the Internet Computer rejects any call that would change state.`
      : $t`When enabled, this session can make changes on your behalf. When disabled, the session is read-only: the Internet Computer rejects any call that would change state.`,
  );
</script>

<div class={["flex flex-row items-center", className]}>
  <Checkbox
    bind:checked={
      () => accessLevel === prompt,
      (checked: boolean) =>
        (accessLevel = checked
          ? prompt
          : prompt === "read-only"
            ? "full-access"
            : "read-only")
    }
    {label}
    size="sm"
    {disabled}
  />
  <Tooltip
    {label}
    {description}
    direction="up"
    align="end"
    offset="0rem"
    class="max-w-80"
  >
    <button
      class="btn btn-tertiary btn-sm btn-icon ms-auto !cursor-default !rounded-full"
      aria-label={$t`More information about the access level`}
    >
      <HelpCircleIcon class="size-5" />
    </button>
  </Tooltip>
</div>
