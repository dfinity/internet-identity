<script lang="ts">
  import { ChevronRightIcon, ShieldAlertIcon } from "@lucide/svelte";
  import type { Incident } from "../status";

  interface Props {
    incident: Incident;
    class?: string;
    onOpen: () => void;
  }

  const { incident, class: className, onOpen }: Props = $props();
</script>

<!-- A phone's incident, once its sheet is put away unanswered: it stays at
     the top of the apps in red, in place of a pill at the bottom that is easy
     to scroll past, and opens the sheet again. It is laid out as a row of the
     panel is, and ends in a row's chevron: across four columns of apps a
     filled button leaves the title too little room to stay on one line. Its
     sides are padded by what the shelf under it leaves beside its logos, so
     the icon and the chevron stand on the logos' edges. -->
<button
  type="button"
  onclick={onOpen}
  aria-label={`${incident.title}. ${incident.question} Review`}
  class={[
    "bg-bg-error-primary ring-border-error_subtle flex items-center gap-3 rounded-3xl px-4 py-3 text-start ring-1 ring-inset @lg:px-8",
    "focus-visible:outline-focus-ring-error focus-visible:outline-2 focus-visible:outline-offset-2",
    "transition-[scale] duration-160 ease-out active:scale-[0.98] motion-reduce:active:scale-100",
    className,
  ]}
>
  <span
    aria-hidden="true"
    class="text-fg-error-primary grid size-9 shrink-0 place-items-center rounded-[0.625rem] bg-(--ii-error-tint)"
  >
    <ShieldAlertIcon class="size-4.5" />
  </span>
  <span class="flex min-w-0 flex-1 flex-col gap-0.5">
    <!-- A longer title, or a narrower phone, breaks into two even lines
         instead of leaving its last word alone on the second. -->
    <span class="text-text-primary text-sm font-semibold text-balance">
      {incident.title}
    </span>
    <span class="text-text-error-primary text-[13px] leading-[18px]">
      {incident.question}
    </span>
  </span>
  <ChevronRightIcon
    aria-hidden="true"
    class="text-fg-error-primary size-4 shrink-0 rtl:-scale-x-100"
  />
</button>
