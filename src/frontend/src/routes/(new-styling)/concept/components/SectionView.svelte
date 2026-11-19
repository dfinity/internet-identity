<script lang="ts" module>
  export type Section = "access" | "recovery" | "devices" | "integrations";
</script>

<script lang="ts">
  import type { Component } from "svelte";
  import {
    BookOpenIcon,
    KeyRoundIcon,
    MailIcon,
    MonitorSmartphoneIcon,
    PlusIcon,
    ServerIcon,
    SquareTerminalIcon,
    UserRoundIcon,
    XIcon,
  } from "@lucide/svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import { formatRelative } from "$lib/stores/locale.store";
  import {
    brandIconOf,
    nameOf,
  } from "../../manage/(authenticated)/devices/browsers";
  import type { MockBrowser, MockIdentity } from "../mockData";
  import {
    accessCount,
    aiAccessOn,
    cliAccessOn,
    emailStatus,
    phraseStatus,
    throughRecovery,
    withAiAccess,
    withCliAccess,
    type CardStatus,
    type Scenario,
    type Tone,
  } from "../status";
  import Elevated from "./Elevated.svelte";
  import type { RecoveryMethod } from "./RecoveryFlow.svelte";

  interface Props {
    section: Section;
    /** `card`: inside its own card, grown in the panel, which names it and
     *  closes it: the lead, then the rows on the card itself. `page`: a view
     *  of its own, as in a phone's sheet: the title, the lead and Close,
     *  then the rows on a card. */
    variant: "card" | "page";
    scenario: Scenario;
    /** What the section's card says. Open, a card that isn't neutral says
     *  the same on the item it's about, with the same cue (Thomas). */
    card?: CardStatus;
    signedInWith: MockIdentity["signedInWith"];
    browsers: MockBrowser[];
    /** Close, at the end of a page's title. */
    onClose?: () => void;
    onSignOut: (id: string) => void;
    /** Moves the scenario on, as finishing the step would. */
    onResolve: (patch: Partial<Scenario>, message: string) => void;
    /** Opens recovery's dialog on one way back in, where it's set up,
     *  checked or managed. */
    onRecovery: (method: RecoveryMethod) => void;
    /** For the steps the concept doesn't walk through. */
    onUnavailable: (step: string) => void;
  }

  const {
    section,
    variant,
    scenario,
    card,
    signedInWith,
    browsers,
    onClose,
    onSignOut,
    onResolve,
    onRecovery,
    onUnavailable,
  }: Props = $props();

  interface Row {
    id: string;
    icon: Component | { src: string };
    title: string;
    meta: string;
    badge?: { label: string; color?: "surface" | "success" };
    /** The card's message is about this row: its word for it ("New",
     *  "Changed"), in the card's style. `dot`: no word by the title; the
     *  row's own line says it after a dot, as a browser's "New sign-in"
     *  does, where a word by the title would say it twice. */
    cue?: { label: string; tone: Tone; dot?: boolean };
    /** Keeps its own line under the cue, where the card's sentence would
     *  only repeat the title ("Recovery phrase" / "…not verified yet"). */
    keepMeta?: boolean;
    /** Not set up yet. */
    empty?: boolean;
    action?: {
      label: string;
      /** `tertiary`: no frame, its text ending where the row's lines do. */
      variant?: "primary" | "secondary" | "tertiary" | "danger";
      onclick: () => void;
    };
  }

  const TITLES: Record<Section, [string, string]> = {
    access: ["Access", "The ways you can sign in."],
    recovery: ["Recovery", "Get back in if you lose access."],
    devices: ["Devices", "Where you're signed in."],
    integrations: ["Integrations", "How other tools connect to your identity."],
  };

  /** The row the card's message is about, and the word its cue says there.
   *  A message about no item (recovery not set up, a way in to add) has
   *  none: the section's lead says it instead. */
  const subject = $derived.by<{ row: string; label: string } | undefined>(
    () => {
      if (card === undefined || card.tone === "neutral") return undefined;
      switch (section) {
        case "access":
          return card.tone !== "warning" && scenario.access === "unused"
            ? { row: "yubikey", label: "Unused" }
            : undefined;
        case "recovery":
          return scenario.recovery === "usedPhrase"
            ? { row: "phrase", label: "In use" }
            : scenario.recovery === "usedEmail"
              ? { row: "email", label: "In use" }
              : scenario.recovery === "phraseReset"
                ? { row: "phrase", label: "Changed" }
                : phraseStatus(scenario.recovery) === "unchecked"
                  ? { row: "phrase", label: "Not verified" }
                  : emailStatus(scenario.recovery) === "pending"
                    ? { row: "email", label: "Pending" }
                    : undefined;
        case "devices":
          return browsers.some(({ isNew }) => isNew === true)
            ? { row: "firefox", label: "New" }
            : undefined;
        case "integrations":
          return scenario.integrations === "local"
            ? { row: "local", label: "Open" }
            : scenario.integrations === "cliConfirm"
              ? { row: "cli", label: "Waiting" }
              : undefined;
      }
    },
  );
  /** The card's message, told by the lead, where it's about no row. */
  const told = $derived(
    card !== undefined && card.tone !== "neutral" && subject === undefined
      ? card
      : undefined,
  );

  /** The row the message is about comes first and says the card's own
   *  sentence, with its cue in place of a badge. A browser's row already
   *  names it ("Firefox on Windows"), so it says the rest, "New sign-in",
   *  with when, after the cue's dot. */
  const withSubject = (rows: Row[]): Row[] => {
    if (subject === undefined || card === undefined) return rows;
    const dot = section === "devices";
    const marked = rows.map((row): Row =>
      row.id === subject.row
        ? {
            ...row,
            meta: dot || row.keepMeta === true ? row.meta : card.sentence,
            badge: undefined,
            cue: { label: subject.label, tone: card.tone, dot },
          }
        : row,
    );
    return [
      ...marked.filter(({ id }) => id === subject.row),
      ...marked.filter(({ id }) => id !== subject.row),
    ];
  };

  const accessRows = $derived.by<Row[]>(() => {
    // A passkey kept on this device alone, as Windows Hello keeps it, doesn't
    // sync anywhere: the one case where losing the device loses the way in.
    const local = scenario.access === "oneLocal";
    const rows: Row[] = [
      {
        id: "current",
        icon: signedInWith.kind === "sso" ? SsoIcon : PasskeyIcon,
        title: local ? "Windows Hello" : signedInWith.title,
        meta: local
          ? "Passkey on this device only · In use now"
          : throughRecovery(scenario.recovery)
            ? signedInWith.meta
            : `${signedInWith.meta} · In use now`,
        badge: throughRecovery(scenario.recovery)
          ? undefined
          : { label: "Active", color: "success" },
      },
    ];
    if (accessCount(scenario.access) > 1) {
      rows.push(
        {
          id: "yubikey",
          icon: KeyRoundIcon,
          title: "YubiKey",
          meta:
            scenario.access === "unused"
              ? "Security key · Last used 8 months ago"
              : "Security key · Last used 3 days ago",
          action:
            scenario.access === "unused"
              ? {
                  label: "Remove",
                  variant: "danger",
                  onclick: () =>
                    onResolve({ access: "calm" }, "YubiKey removed"),
                }
              : undefined,
        },
        {
          id: "google",
          icon: UserRoundIcon,
          title: "Google",
          meta: "t•••@gmail.com",
        },
      );
    }
    return rows;
  });

  /** Both ways back in, said as recovery's dialog says them, each opening
   *  the dialog on itself. Only a first way, when there's none, asks loudly:
   *  one way is enough, so the other never does. */
  const recoveryRows = $derived.by<Row[]>(() => {
    const ways = [
      {
        id: "phrase" as const,
        icon: BookOpenIcon,
        title: "Recovery phrase",
        status: phraseStatus(scenario.recovery),
        off: "12 words you write down and keep offline",
        on: "Checked on 14 Nov",
      },
      {
        id: "email" as const,
        icon: MailIcon,
        title: "Recovery email",
        status: emailStatus(scenario.recovery),
        off: "A code to your inbox gets you back in",
        on: "t•••@gmail.com",
      },
    ];
    // Only a first way, or the step the card urges, asks loudly.
    const loud = ways.every((way) => way.status === "off")
      ? ways[0]
      : card?.tone === "warning"
        ? ways.find((way) => way.status === "unchecked")
        : undefined;
    return ways.map((way): Row => {
      const { status } = way;
      return {
        id: way.id,
        icon: way.icon,
        title: way.title,
        meta: {
          off: way.off,
          unchecked: "Written down, not checked yet",
          pending: "Code sent to t•••@gmail.com",
          inUse: "You're signed in with it now",
          new: "Reset 2 hours ago",
          on: way.on,
        }[status],
        keepMeta:
          status === "unchecked" || status === "pending" || status === "inUse",
        badge:
          status === "inUse"
            ? { label: "In use", color: "success" }
            : status === "on" || status === "new"
              ? { label: "On", color: "success" }
              : undefined,
        empty: status === "off",
        action: {
          label: {
            off: "Set up",
            unchecked: "Verify",
            pending: "Enter code",
            on: "Manage",
            inUse: "Manage",
            new: "Manage",
          }[status],
          variant: way === loud ? "primary" : undefined,
          onclick: () => onRecovery(way.id),
        },
      };
    });
  });

  const integrationRows = $derived.by<Row[]>(() => {
    const { integrations } = scenario;
    const ai = aiAccessOn(integrations);
    const rows: Row[] = [];
    if (ai) {
      rows.push({
        id: "mcp",
        icon: McpIcon,
        title: "Internet Computer MCP",
        meta: "Official · Hosted by DFINITY",
        action: {
          label: "Customize",
          onclick: () => onUnavailable("Customizing the connector"),
        },
      });
    } else {
      rows.push({
        id: "ai",
        icon: McpIcon,
        title: "AI access",
        meta: "Chat with AI to act across your apps",
        empty: true,
        action: {
          label: "Set up",
          onclick: () =>
            onResolve(
              { integrations: withAiAccess(integrations, true) },
              "AI access on",
            ),
        },
      });
    }
    // Kept on purpose, the server is still there, only no longer flagged.
    if (integrations === "local" || integrations === "localAllowed") {
      rows.push({
        id: "local",
        icon: ServerIcon,
        title: "Local server",
        meta: "Runs on this computer · Any local program can connect",
        action: {
          label: "Remove",
          variant: "danger",
          onclick: () =>
            onResolve({ integrations: "calm" }, "Local server removed"),
        },
      });
    }
    rows.push(
      integrations === "cliConfirm"
        ? {
            id: "cli",
            icon: SquareTerminalIcon,
            title: "CLI access",
            meta: "The ICP CLI is waiting for approval on this device",
            action: {
              label: "Approve",
              variant: "primary",
              onclick: () =>
                onResolve({ integrations: "calm" }, "ICP CLI approved"),
            },
          }
        : cliAccessOn(integrations)
          ? {
              id: "cli",
              icon: SquareTerminalIcon,
              title: "CLI access",
              meta: "On for this device",
              badge: { label: "On", color: "success" },
            }
          : {
              id: "cli",
              icon: SquareTerminalIcon,
              title: "CLI access",
              meta: "Sign in from the ICP CLI on a device you trust",
              empty: true,
              action: {
                label: "Turn on",
                onclick: () =>
                  onResolve(
                    { integrations: withCliAccess(integrations, true) },
                    "CLI access on",
                  ),
              },
            },
    );
    return rows;
  });

  /** One row a browser, named whole ("Firefox on Windows"), with no
   *  platform headings: this browser first, then the most recently used.
   *  The one the card's message is about goes above them all. */
  const browserRows = $derived(
    [...browsers]
      .sort((one, other) =>
        one.isCurrent === other.isCurrent
          ? other.lastUsedMillis - one.lastUsedMillis
          : one.isCurrent === true
            ? -1
            : 1,
      )
      .map((browser): Row => {
        const icon = brandIconOf(browser.description);
        const isCurrent = browser.isCurrent === true;
        const when = $formatRelative(new Date(browser.lastUsedMillis), {
          style: "long",
        });
        return {
          id: browser.id,
          icon: icon !== undefined ? { src: icon } : MonitorSmartphoneIcon,
          title: nameOf(browser.description),
          // "Active now" only where it's true: the page is read here. Another
          // browser's last use is known to five minutes at best.
          meta: isCurrent
            ? "This browser · Active now"
            : browser.isNew === true
              ? `New sign-in · ${when}`
              : `Last used ${when}`,
          action: isCurrent
            ? undefined
            : {
                label: "Sign out",
                variant: "tertiary",
                onclick: () => onSignOut(browser.id),
              },
        };
      }),
  );

  /** A cue's pill, as recovery's dialog draws a way back in: the style's
   *  own tint for a warning or a risk, the plain pill with the text's own
   *  dot for info. */
  const CUE: Record<Tone, string> = {
    risk: "text-fg-error-primary border-transparent bg-(--ii-error-tint)",
    warning:
      "text-fg-warning-primary border-transparent bg-(--ii-warning-tint)",
    info: "border-border-tertiary bg-bg-primary text-text-primary",
    neutral: "border-border-tertiary bg-bg-primary text-text-tertiary",
  };
  /** What a message says in each style, on a row or in the lead. */
  const SAID: Record<Tone, string> = {
    risk: "text-text-error-primary",
    warning: "text-warning-700 dark:text-text-warning-primary",
    info: "text-text-primary",
    neutral: "text-text-tertiary",
  };
</script>

{#snippet lead()}
  <!-- What the section holds; a message about no row (recovery not set up,
       a way in to add) takes its place, in its card's style. -->
  {#if told !== undefined}
    <span
      class={[
        "flex items-center gap-1.5 text-[13px] leading-[18px] text-pretty",
        SAID[told.tone],
      ]}
    >
      {#if told.tone === "info"}
        <span class="bg-fg-primary size-1.5 shrink-0 rounded-full"></span>
      {/if}
      {told.sentence}
    </span>
  {:else}
    {TITLES[section][1]}
  {/if}
{/snippet}

{#snippet row(item: Row)}
  <li class="flex items-center gap-3 px-4 py-3.5">
    <span
      aria-hidden="true"
      class={[
        "flex size-9 shrink-0 items-center justify-center rounded-lg",
        item.empty === true
          ? "border-border-secondary text-fg-quaternary border border-dashed"
          : "text-fg-secondary bg-(--ii-selected)",
      ]}
    >
      {#if "src" in item.icon}
        <img src={item.icon.src} alt="" class="size-5" />
      {:else}
        {@const Icon = item.icon}
        <Icon class="size-4.5" />
      {/if}
    </span>
    <span class="flex min-w-0 flex-1 flex-col gap-0.5">
      <span class="flex flex-wrap items-center gap-x-2 gap-y-0.5">
        <span class="text-text-primary text-sm font-medium">{item.title}</span>
        {#if item.cue !== undefined && item.cue.dot !== true}
          <span
            class={[
              "inline-flex items-center gap-1 rounded-full border py-0.5 ps-1.5 pe-2 text-xs font-medium",
              CUE[item.cue.tone],
            ]}
          >
            <span class="size-1.5 shrink-0 rounded-full bg-current"></span>
            {item.cue.label}
          </span>
        {:else if item.badge !== undefined}
          <Badge
            size="sm"
            color={item.badge.color}
            dot={item.badge.color === "success"}>{item.badge.label}</Badge
          >
        {/if}
      </span>
      <span
        class={[
          "text-[13px] leading-[18px] text-pretty",
          item.cue?.dot === true && "flex items-start gap-1.5",
          SAID[item.cue?.tone ?? "neutral"],
        ]}
      >
        {#if item.cue?.dot === true}
          <!-- On the first line's middle, where a narrow sheet wraps it. -->
          <span class="mt-1.5 size-1.5 shrink-0 rounded-full bg-current"></span>
        {/if}
        {item.meta}
      </span>
    </span>
    {#if item.action !== undefined}
      <button
        type="button"
        onclick={item.action.onclick}
        class={[
          "btn btn-sm shrink-0 font-medium",
          item.action.variant === "primary"
            ? "btn-primary"
            : item.action.variant === "tertiary"
              ? "btn-tertiary -me-3"
              : "btn-secondary",
          item.action.variant === "danger" && "btn-danger",
        ]}
      >
        {item.action.label}
      </button>
    {/if}
  </li>
{/snippet}

<!-- The rows lie on the grown card itself, or, on a page, on a card of their
     own. -->
{#snippet rows(items: Row[])}
  <ul class="ii-rows flex flex-col [--ii-rows-inset:4rem]">
    {#each items as item (item.id)}
      {@render row(item)}
    {/each}
  </ul>
{/snippet}

{#snippet body()}
  {@render rows(
    withSubject(
      section === "access"
        ? accessRows
        : section === "recovery"
          ? recoveryRows
          : section === "devices"
            ? browserRows
            : integrationRows,
    ),
  )}
{/snippet}

{#snippet addMethod()}
  <button
    type="button"
    onclick={() => onUnavailable("Adding an access method")}
    class={[
      "btn w-full font-medium",
      // The step Access urges: its card's message is the lead above.
      told?.tone === "warning" ? "btn-primary" : "btn-secondary",
    ]}
  >
    <PlusIcon class="size-4" />
    Add access method
  </button>
{/snippet}

{#if variant === "card"}
  <!-- Under the card's title, where its status sentence was. -->
  <div class="flex flex-col pt-2.5">
    <p class="text-text-tertiary px-4 text-[13px] leading-[18px]">
      {@render lead()}
    </p>
    <div class="flex flex-col pt-1">
      {@render body()}
    </div>
    {#if section === "access"}
      <div class="px-4 pt-1 pb-4">{@render addMethod()}</div>
    {:else}
      <div class="h-1"></div>
    {/if}
  </div>
{:else}
  <div class="flex flex-col gap-4">
    <!-- Only what this is and the way out: Close at the end, across from the
         title, as recovery's dialog has it. -->
    <header class="flex items-start gap-3">
      <div class="flex min-w-0 flex-1 flex-col gap-0.5 pt-1.5">
        <h2 class="text-text-primary text-base leading-6 font-semibold">
          {TITLES[section][0]}
        </h2>
        <p class="text-text-tertiary text-[13px] leading-[18px]">
          {@render lead()}
        </p>
      </div>
      <button
        type="button"
        onclick={onClose}
        class="btn btn-tertiary btn-icon btn-sm -me-2 shrink-0 rounded-full"
      >
        <XIcon class="size-5" aria-hidden="true" />
        <span>Close</span>
      </button>
    </header>
    <Elevated offset={1} class="flex flex-col overflow-hidden rounded-2xl">
      {@render body()}
    </Elevated>
    {#if section === "access"}
      {@render addMethod()}
    {/if}
  </div>
{/if}
