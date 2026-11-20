<script lang="ts">
  import {
    BotIcon,
    BriefcaseMedicalIcon,
    KeyRoundIcon,
    MonitorSmartphoneIcon,
    ShieldAlertIcon,
  } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import type { Incident, IncidentAnswer } from "./status";
  import Elevated from "../../foundation/surfaces/Elevated.svelte";

  interface Props {
    incident: Incident;
    /** `panel` heads the identity panel; `sheet` meets the user on a phone,
     *  laid out as the product's confirmation sheets are. */
    variant?: "panel" | "sheet";
    onAnswer: (answer: IncidentAnswer) => void;
  }

  const { incident, variant = "panel", onAnswer }: Props = $props();

  // The glyphs of the panel's rows, so each fact points at the row it changed.
  const ICON = {
    access: KeyRoundIcon,
    devices: MonitorSmartphoneIcon,
    recovery: BriefcaseMedicalIcon,
    apps: BotIcon,
    integrations: BotIcon,
  };

  const titleId = $props.id();
  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
</script>

{#if variant === "sheet"}
  <!-- A phone has no room beside the apps, so a risk meets the user here,
       over them, before anything else: what happened, then the answer. The
       answer that protects the identity leads, and says what it will do. -->
  <section aria-labelledby={titleId} class="flex flex-col gap-6">
    <div class="flex flex-col gap-4">
      <FeaturedIcon size="lg" variant="error">
        <ShieldAlertIcon class="size-6" aria-hidden="true" />
      </FeaturedIcon>
      <div class="flex flex-col gap-2">
        <!-- The sheet opens on its title, not on an answer: either answer
             changes the identity, so a stray Enter mustn't give one. -->
        <!-- svelte-ignore a11y_autofocus -->
        <h2
          id={titleId}
          tabindex="-1"
          autofocus
          class="text-text-primary text-2xl font-medium text-balance outline-none"
        >
          {incident.title}
        </h2>
        <p class="text-text-error-primary text-base font-medium">
          {incident.question}
        </p>
      </div>
    </div>
    <ol
      class="border-border-secondary divide-border-secondary flex flex-col divide-y rounded-xl border"
    >
      {#each incident.facts as fact (fact.text)}
        {@const Icon = ICON[fact.kind]}
        <li class="flex items-center gap-3 px-4 py-3 text-sm">
          <Icon class="text-fg-tertiary size-4 shrink-0" aria-hidden="true" />
          <span class="text-text-primary min-w-0 flex-1">{fact.text}</span>
          <span class="text-text-tertiary shrink-0 tabular-nums">
            {fact.meta}
          </span>
        </li>
      {/each}
    </ol>
    <div class="flex flex-col gap-3">
      <button
        type="button"
        onclick={() => onAnswer(incident.secure)}
        class={["btn btn-danger w-full", PRESS]}
      >
        {incident.secure.label}
      </button>
      <button
        type="button"
        onclick={() => onAnswer(incident.confirm)}
        class={["btn btn-tertiary w-full", PRESS]}
      >
        {incident.confirm.label}
      </button>
      <p class="text-text-tertiary px-4 text-center text-xs text-pretty">
        {incident.secure.detail}
      </p>
    </div>
  </section>
{:else}
  <!-- Heads the panel while it's unanswered: the story the red rows below
       are part of, told once, and the only place that asks about it. The
       header reads as a row of the panel; what happened follows at the rows'
       text inset, and the two answers close the card. -->
  <Elevated
    offset={1}
    as="section"
    aria-labelledby={titleId}
    class="flex flex-col gap-3 overflow-hidden rounded-2xl p-4"
  >
    <div class="flex items-start gap-3">
      <span
        aria-hidden="true"
        class="text-fg-error-primary grid size-9 shrink-0 place-items-center rounded-[0.625rem] bg-(--ii-error-tint)"
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
          <Icon
            class="text-fg-quaternary size-3.5 shrink-0"
            aria-hidden="true"
          />
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
        class={["btn btn-danger btn-sm flex-1 rounded-full font-medium", PRESS]}
      >
        {incident.secure.label}
      </button>
      <button
        type="button"
        onclick={() => onAnswer(incident.confirm)}
        class={[
          "btn btn-secondary btn-sm flex-1 rounded-full font-medium",
          PRESS,
        ]}
      >
        {incident.confirm.label}
      </button>
    </div>
    <p class="text-text-tertiary text-xs text-pretty">
      {incident.secure.detail}
    </p>
  </Elevated>
{/if}
