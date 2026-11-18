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
  import GroupHeading from "../../manage/(authenticated)/devices/components/GroupHeading.svelte";
  import {
    brandIconOf,
    brandNameOf,
    kindOf,
    platformWordOf,
  } from "../../manage/(authenticated)/devices/browsers";
  import type { MockBrowser, MockIdentity } from "../mockData";
  import {
    aiAccessOn,
    cliAccessOn,
    emailStatus,
    phraseStatus,
    withAiAccess,
    withCliAccess,
    type MethodStatus,
    type Scenario,
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
    /** Why this row is worth a look: a warning or a risk. */
    flag?: "warning" | "error";
    /** Not set up yet. */
    empty?: boolean;
    action?: {
      label: string;
      variant?: "primary" | "secondary" | "danger";
      onclick: () => void;
    };
  }

  const TITLES: Record<Section, [string, string]> = {
    access: ["Access", "The ways you can sign in."],
    recovery: ["Recovery", "Get back in if you lose access."],
    devices: ["Devices", "Where you're signed in."],
    integrations: ["Integrations", "How other tools connect to your identity."],
  };

  const accessRows = $derived.by<Row[]>(() => {
    const rows: Row[] = [
      {
        id: "current",
        icon: signedInWith.kind === "sso" ? SsoIcon : PasskeyIcon,
        title: signedInWith.title,
        meta:
          scenario.recovery === "usedPhrase"
            ? signedInWith.meta
            : `${signedInWith.meta} · In use now`,
        badge:
          scenario.recovery === "usedPhrase"
            ? undefined
            : { label: "Active", color: "success" },
      },
    ];
    if (scenario.access !== "one") {
      rows.push(
        {
          id: "yubikey",
          icon: KeyRoundIcon,
          title: "YubiKey",
          meta:
            scenario.access === "unused"
              ? "Security key · Last used 8 months ago"
              : "Security key · Last used 3 days ago",
          flag: scenario.access === "unused" ? "warning" : undefined,
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
    if (scenario.access === "newPasskey") {
      rows.push({
        id: "new-passkey",
        icon: PasskeyIcon,
        title: "Passkey",
        meta: "Added from Firefox on Windows · 10 min ago",
        badge: { label: "New" },
        flag: "warning",
        action: {
          label: "Remove",
          variant: "danger",
          onclick: () => onResolve({ access: "calm" }, "Passkey removed"),
        },
      });
    }
    return rows;
  });

  /** Both ways back in, said as recovery's dialog says them, each opening
   *  the dialog on itself. Only a step left half done, or a first way when
   *  there's none, asks loudly. */
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
    const halfDone = (status: MethodStatus) =>
      status === "unchecked" || status === "pending";
    const loud =
      ways.find((way) => halfDone(way.status)) ??
      (ways.every((way) => way.status === "off") ? ways[0] : undefined);
    return ways.map((way): Row => {
      const { status } = way;
      return {
        id: way.id,
        icon: way.icon,
        title: way.title,
        meta: {
          off: way.off,
          unchecked: "Written down, not checked yet",
          pending: "Waiting for the code from your inbox",
          inUse: "You're signed in with it now",
          new: "Reset 2 hours ago",
          on: way.on,
        }[status],
        badge:
          status === "inUse"
            ? { label: "In use", color: "success" }
            : status === "on" || status === "new"
              ? { label: "On", color: "success" }
              : undefined,
        flag: halfDone(status) ? "warning" : undefined,
        empty: status === "off",
        action: {
          label:
            status === "off"
              ? "Set up"
              : status === "unchecked"
                ? "Verify"
                : status === "pending"
                  ? "Continue"
                  : "Manage",
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
        flag: integrations === "local" ? "error" : undefined,
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
            flag: "warning",
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

  const deviceGroups = $derived.by(() => {
    const groups: { platform: string; browsers: MockBrowser[] }[] = [];
    for (const browser of browsers) {
      const platform = platformWordOf(browser.description);
      const group = groups.find((g) => g.platform === platform);
      if (group === undefined) {
        groups.push({ platform, browsers: [browser] });
      } else {
        group.browsers.push(browser);
      }
    }
    return groups;
  });

  const browserRow = (browser: MockBrowser): Row => {
    const icon = brandIconOf(browser.description);
    const isCurrent = browser.isCurrent === true;
    const isNew = browser.isNew === true;
    return {
      id: browser.id,
      icon: icon !== undefined ? { src: icon } : MonitorSmartphoneIcon,
      title: brandNameOf(browser.description),
      meta: isCurrent
        ? "Right now"
        : isNew
          ? `First seen ${$formatRelative(new Date(browser.lastUsedMillis), { style: "long" })}`
          : `Last used ${$formatRelative(new Date(browser.lastUsedMillis), { style: "long" })}`,
      badge: isCurrent
        ? { label: "This browser" }
        : isNew
          ? { label: "New" }
          : undefined,
      flag: isNew || browser.isStale === true ? "warning" : undefined,
      action: isCurrent
        ? undefined
        : { label: "Sign out", onclick: () => onSignOut(browser.id) },
    };
  };
</script>

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
        {#if item.badge !== undefined}
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
          item.flag === "error"
            ? "text-text-error-primary"
            : item.flag === "warning"
              ? "text-warning-700 dark:text-text-warning-primary"
              : "text-text-tertiary",
        ]}
      >
        {item.meta}
      </span>
    </span>
    {#if item.action !== undefined}
      <button
        type="button"
        onclick={item.action.onclick}
        class={[
          "btn btn-sm shrink-0 font-medium",
          item.action.variant === "primary" ? "btn-primary" : "btn-secondary",
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
  {#if section === "devices"}
    {#each deviceGroups as group (group.platform)}
      <GroupHeading
        kind={kindOf(group.browsers[0].description)}
        platform={group.platform}
        count={group.browsers.length}
      />
      {@render rows(group.browsers.map(browserRow))}
    {/each}
  {:else}
    {@render rows(
      section === "access"
        ? accessRows
        : section === "recovery"
          ? recoveryRows
          : integrationRows,
    )}
  {/if}
{/snippet}

{#snippet addMethod()}
  <button
    type="button"
    onclick={() => onUnavailable("Adding an access method")}
    class="btn btn-secondary w-full font-medium"
  >
    <PlusIcon class="size-4" />
    Add access method
  </button>
{/snippet}

{#if variant === "card"}
  <!-- Under the card's title, where its status sentence was. -->
  <div class="flex flex-col pt-2.5">
    <p class="text-text-tertiary px-4 text-[13px] leading-[18px]">
      {TITLES[section][1]}
    </p>
    <div class={["flex flex-col", section !== "devices" && "pt-1"]}>
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
          {TITLES[section][1]}
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
      {#if section === "devices"}
        <div class="h-1"></div>
      {/if}
    </Elevated>
    {#if section === "access"}
      {@render addMethod()}
    {/if}
  </div>
{/if}
