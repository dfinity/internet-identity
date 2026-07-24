<script lang="ts">
  import Select from "$lib/components/ui/Select.svelte";
  import { ChevronDownIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import {
    cappedSessionDurations,
    formatSessionDuration,
  } from "$lib/utils/sessionDuration";

  type Align = "start" | "center" | "end";

  interface Props {
    /** The largest duration (seconds) the user may pick. Presets longer than it
     *  are hidden, and — when it isn't itself a preset — it's offered as the top
     *  option so the full allowed duration stays selectable (e.g. a 2-hour cap
     *  shows a "2h" option). */
    maxSeconds: number;
    /** The selected duration in seconds. Bindable so the parent owns the value. */
    value: number;
    /** Disable the trigger (e.g. while authorizing) so the dropdown can't open. */
    disabled?: boolean;
    /** Popover alignment relative to the trigger. */
    align?: Align;
  }

  let {
    maxSeconds,
    value = $bindable(),
    disabled = false,
    align = "end",
  }: Props = $props();

  const MINUTE = 60;
  const HOUR = 60 * MINUTE;
  const DAY = 24 * HOUR;
  const WEEK = 7 * DAY;

  // The named durations the picker offers, spanning the allowed range (10
  // minutes to 30 days). `$derived` so the labels re-translate when the locale
  // changes.
  const presets = $derived([
    { value: 10 * MINUTE, label: $t`10 minutes` },
    { value: HOUR, label: $t`1 hour` },
    { value: 8 * HOUR, label: $t`8 hours` },
    { value: DAY, label: $t`1 day` },
    { value: WEEK, label: $t`1 week` },
    { value: 30 * DAY, label: $t`30 days` },
  ]);

  // A named preset's label, or the compact format for an off-preset duration
  // (e.g. an off-preset cap shown as "2h").
  const labelFor = (seconds: number): string =>
    presets.find((preset) => preset.value === seconds)?.label ??
    formatSessionDuration(seconds);

  // Every preset up to the cap, plus the cap itself when it isn't a preset, each
  // with its label and a flag for the current selection.
  const options = $derived(
    cappedSessionDurations(
      maxSeconds,
      presets.map((preset) => preset.value),
    ).map((optionValue) => ({
      value: optionValue,
      label: labelFor(optionValue),
      selected: optionValue === value,
    })),
  );

  // The trigger shows the selected duration's label.
  const selectedLabel = $derived(labelFor(value));
</script>

<Select {options} onChange={(selected) => (value = selected)} {align}>
  <!-- Disabled while authorizing: a disabled button dispatches no click, so the
       Select can't open once the user has committed. -->
  <button
    type="button"
    class="btn btn-secondary btn-sm shrink-0 gap-2"
    {disabled}
  >
    <span>{selectedLabel}</span>
    <ChevronDownIcon class="size-4" />
  </button>
</Select>
