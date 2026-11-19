<script lang="ts">
  import type { Component } from "svelte";
  import { XIcon } from "@lucide/svelte";
  import { browsersFor, type MockIdentity } from "../mockData";
  import { glyphsOf } from "../glyphs";
  import {
    deriveStatus,
    type CardKind,
    type Scenario,
    type Tone,
  } from "../status";
  import Elevated from "./Elevated.svelte";
  import HeadCard from "./HeadCard.svelte";
  import SectionView, { type Section } from "./SectionView.svelte";
  import StatusCard from "./StatusCard.svelte";
  import SurfaceLevel from "./SurfaceLevel.svelte";

  interface Props {
    titles: Record<CardKind, string>;
    icons: Record<CardKind, Component>;
    /** What the identity signed in with, for the Access section's rows. */
    signedInWith: MockIdentity["signedInWith"];
    onClose: () => void;
  }

  const { titles, icons, signedInWith, onClose }: Props = $props();

  type Kind = "access" | "recovery" | "devices";

  /** Thomas's list, card by card, in his words, each with the scenario that
   *  shows it. The last Devices state is the one he pairs with a changed
   *  recovery: together they're the strong warning. */
  const STATES: {
    kind: Kind;
    examples: { label: string; scenario: Partial<Scenario> }[];
  }[] = [
    {
      kind: "access",
      examples: [
        { label: "X ways to sign in", scenario: {} },
        { label: "X unused for X time", scenario: { access: "unused" } },
        {
          label: "Add a new method, signed in with recovery",
          scenario: { recovery: "usedPhrase" },
        },
      ],
    },
    {
      kind: "recovery",
      examples: [
        {
          label: "Not set up",
          scenario: { recovery: "none", access: "one" },
        },
        {
          label: "Not set up, single unsynced passkey",
          scenario: { recovery: "none", access: "oneLocal" },
        },
        { label: "Set-up phrase", scenario: { recovery: "phraseOnly" } },
        { label: "Set-up email", scenario: { recovery: "emailOnly" } },
        { label: "Set-up both", scenario: {} },
        {
          label: "Phrase not verified",
          scenario: { recovery: "unverified" },
        },
        {
          label: "Phrase not verified, email set up",
          scenario: { recovery: "unverifiedWithEmail" },
        },
        {
          label: "Email pending",
          scenario: { recovery: "emailPending" },
        },
        {
          label: "You're signed in through recovery, with the phrase",
          scenario: { recovery: "usedPhrase" },
        },
        {
          label: "You're signed in through recovery, with the email",
          scenario: { recovery: "usedEmail" },
        },
        {
          label: "Recovery was changed by new browser",
          scenario: { recovery: "phraseReset", devices: "newBrowser" },
        },
      ],
    },
    {
      kind: "devices",
      examples: [
        { label: "New sign-in from X", scenario: { devices: "newBrowser" } },
        {
          label: "New sign-in from X, with recovery changed",
          scenario: { recovery: "phraseReset", devices: "newBrowser" },
        },
        { label: "Signed in on X browsers", scenario: {} },
      ],
    },
  ];

  const CALM: Scenario = {
    recovery: "calm",
    access: "calm",
    devices: "calm",
    integrations: "calm",
  };

  const showing = (kind: Kind, partial: Partial<Scenario>) => {
    const scenario = { ...CALM, ...partial };
    const browsers = browsersFor(scenario.devices);
    const card = deriveStatus({
      scenario,
      signedOut: new Set(),
      browserCount: browsers.length,
      apps: { total: 0, notifying: 0 },
    }).find((one) => one.kind === kind);
    if (card === undefined) throw new Error(`No ${kind} card`);
    return {
      scenario,
      browsers,
      card,
      glyphs: glyphsOf(card, scenario, browsers, signedInWith.title),
    };
  };

  const STYLE: Record<Tone, { label: string; class: string }> = {
    neutral: {
      label: "Neutral",
      class: "border-border-tertiary bg-bg-primary text-text-tertiary",
    },
    info: {
      label: "Info",
      class: "border-border-tertiary bg-bg-primary text-text-primary",
    },
    warning: {
      label: "Warning",
      class:
        "text-fg-warning-primary border-transparent bg-(--ii-warning-tint)",
    },
    risk: {
      label: "Risk",
      class: "text-fg-error-primary border-transparent bg-(--ii-error-tint)",
    },
  };

  const COLUMNS =
    "grid grid-cols-[13rem_22rem_22rem_8.25rem] items-start gap-x-8";

  const noop = () => {};
  const titleId = $props.id();
</script>

<svelte:window onkeydown={(event) => event.key === "Escape" && onClose()} />

<!-- Concept only: every state of the three cards at once, for Thomas, drawn
     by the cards themselves: the panel's card, the same card open on what it
     says, and the phone's head card on the canvas. Nothing in it is live. -->
<div
  role="dialog"
  aria-modal="true"
  aria-labelledby={titleId}
  class="ii-surface-1 fixed inset-0 z-50 overflow-auto overscroll-contain"
>
  <div class="mx-auto flex w-max flex-col gap-12 px-8 pt-8 pb-16">
    <header class="flex items-start gap-4">
      <div class="flex min-w-0 flex-1 flex-col gap-1">
        <h2
          id={titleId}
          class="text-text-primary text-xl font-semibold tracking-tight"
        >
          Card states
        </h2>
        <p class="text-text-tertiary text-sm">
          Thomas's list, each in one of four styles: neutral, info, warning,
          risk.
        </p>
      </div>
      <!-- svelte-ignore a11y_autofocus -->
      <button
        type="button"
        onclick={onClose}
        autofocus
        class="btn btn-tertiary btn-icon rounded-full"
      >
        <XIcon class="size-5" aria-hidden="true" />
        <span>Close</span>
      </button>
    </header>

    {#each STATES as group (group.kind)}
      <section class="flex flex-col gap-4">
        <h3 class="text-text-primary text-base font-semibold">
          {titles[group.kind]}
        </h3>
        <div
          class={[
            COLUMNS,
            "text-text-tertiary text-xs font-medium tracking-wide uppercase",
          ]}
        >
          <span>State</span>
          <span>Panel</span>
          <span>Open</span>
          <span>Phone</span>
        </div>
        {#each group.examples as example (example.label)}
          {@const shown = showing(group.kind, example.scenario)}
          <div class={[COLUMNS, "border-border-secondary border-t pt-5"]}>
            <div class="flex flex-col items-start gap-2">
              <span class="text-text-primary text-sm font-medium">
                {example.label}
              </span>
              <span
                class={[
                  "inline-flex items-center gap-1 rounded-full border py-0.5 ps-1.5 pe-2 text-xs font-medium",
                  STYLE[shown.card.tone].class,
                ]}
              >
                <span class="size-1.5 shrink-0 rounded-full bg-current"></span>
                {STYLE[shown.card.tone].label}
              </span>
            </div>
            <div inert>
              <SurfaceLevel level={2}>
                <Elevated offset={1} class="overflow-hidden rounded-2xl">
                  <StatusCard
                    kind={group.kind}
                    title={titles[group.kind]}
                    sentence={shown.card.sentence}
                    tone={shown.card.tone}
                    glyphs={shown.glyphs}
                    onOpen={noop}
                  />
                </Elevated>
              </SurfaceLevel>
            </div>
            <div inert>
              <SurfaceLevel level={2}>
                <Elevated offset={1} class="overflow-hidden rounded-2xl">
                  <StatusCard
                    kind={group.kind}
                    title={titles[group.kind]}
                    sentence={shown.card.sentence}
                    tone={shown.card.tone}
                    glyphs={shown.glyphs}
                    open
                    onOpen={noop}
                    onClose={noop}
                  >
                    <SectionView
                      section={group.kind as Section}
                      variant="card"
                      scenario={shown.scenario}
                      card={shown.card}
                      {signedInWith}
                      browsers={shown.browsers}
                      onSignOut={noop}
                      onResolve={noop}
                      onRecovery={noop}
                      onUnavailable={noop}
                    />
                  </StatusCard>
                </Elevated>
              </SurfaceLevel>
            </div>
            <!-- On the canvas, at a 390px phone's width for one of three. -->
            <div inert class="ii-surface-3 rounded-2xl p-2.5">
              <SurfaceLevel level={3}>
                <HeadCard
                  card={shown.card}
                  title={titles[group.kind]}
                  icon={icons[group.kind]}
                />
              </SurfaceLevel>
            </div>
          </div>
        {/each}
      </section>
    {/each}
  </div>
</div>
