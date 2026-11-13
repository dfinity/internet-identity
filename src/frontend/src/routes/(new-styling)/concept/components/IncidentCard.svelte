<script lang="ts">
  import {
    BotIcon,
    BriefcaseMedicalIcon,
    KeyRoundIcon,
    MonitorSmartphoneIcon,
    ShieldAlertIcon,
  } from "@lucide/svelte";
  import type { Incident, IncidentAnswer } from "../status";
  import Elevated from "./Elevated.svelte";

  interface Props {
    incident: Incident;
    onAnswer: (answer: IncidentAnswer) => void;
  }

  const { incident, onAnswer }: Props = $props();

  // The glyphs of the panel's rows, so each fact points at the row it changed.
  const ICON = {
    access: KeyRoundIcon,
    devices: MonitorSmartphoneIcon,
    recovery: BriefcaseMedicalIcon,
    apps: BotIcon,
    integrations: BotIcon,
  };

  const titleId = $props.id();
</script>

<!-- Heads the panel while it's unanswered: the story the red rows below are
     part of, told once, and the only place that asks about it. The header
     reads as a row of the panel; what happened follows at the rows' text
     inset, and the two answers close the card. -->
<Elevated
  offset={1}
  as="section"
  aria-labelledby={titleId}
  class="flex flex-col gap-3 overflow-hidden rounded-2xl p-4"
>
  <div class="flex items-start gap-3">
    <span
      aria-hidden="true"
      class="bg-bg-error-secondary text-fg-error-primary grid size-9 shrink-0 place-items-center rounded-[0.625rem]"
    >
      <ShieldAlertIcon class="size-4.5" />
    </span>
    <div class="flex min-w-0 flex-col gap-0.5">
      <h2 id={titleId} class="text-text-primary text-sm font-semibold">
        {incident.title}
      </h2>
      <p class="text-text-error-primary text-[13px] leading-[18px]">
        {incident.question}
      </p>
    </div>
  </div>
  <ol class="flex flex-col gap-2 ps-12">
    {#each incident.facts as fact (fact.text)}
      {@const Icon = ICON[fact.kind]}
      <li class="flex items-center gap-2 text-[13px] leading-[18px]">
        <Icon class="text-fg-quaternary size-3.5 shrink-0" aria-hidden="true" />
        <span class="text-text-secondary min-w-0 flex-1">{fact.text}</span>
        <span class="text-text-tertiary shrink-0 text-xs tabular-nums">
          {fact.meta}
        </span>
      </li>
    {/each}
  </ol>
  <div class="mt-1 flex gap-2">
    <button
      type="button"
      onclick={() => onAnswer(incident.secure)}
      class="btn btn-danger btn-sm flex-1 rounded-full font-medium transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100"
    >
      {incident.secure.label}
    </button>
    <button
      type="button"
      onclick={() => onAnswer(incident.confirm)}
      class="btn btn-secondary btn-sm flex-1 rounded-full font-medium transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100"
    >
      {incident.confirm.label}
    </button>
  </div>
  <p class="text-text-tertiary text-xs text-pretty">
    {incident.secure.detail}
  </p>
</Elevated>
