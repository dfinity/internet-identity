<script lang="ts">
  import { ShieldAlertIcon } from "@lucide/svelte";
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
     to scroll past, and opens the sheet again. -->
<button
  type="button"
  onclick={onOpen}
  aria-label={`${incident.title}. ${incident.question} Review`}
  class={[
    "bg-bg-error-primary ring-border-error_subtle flex items-center gap-3 rounded-[1.25rem] py-3 ps-3 pe-3 text-start ring-1 ring-inset",
    "focus-visible:outline-focus-ring-error focus-visible:outline-2 focus-visible:outline-offset-2",
    "transition-[scale] duration-160 ease-out active:scale-[0.98] motion-reduce:active:scale-100",
    className,
  ]}
>
  <span
    aria-hidden="true"
    class="bg-bg-error-secondary text-fg-error-primary grid size-9 shrink-0 place-items-center rounded-[0.625rem]"
  >
    <ShieldAlertIcon class="size-4.5" />
  </span>
  <span class="flex min-w-0 flex-1 flex-col gap-0.5">
    <span class="text-text-primary text-sm font-semibold">
      {incident.title}
    </span>
    <span class="text-text-error-primary text-[13px] leading-[18px]">
      {incident.question}
    </span>
  </span>
  <span
    aria-hidden="true"
    class="btn btn-danger btn-sm shrink-0 rounded-full font-medium"
  >
    Review
  </span>
</button>
