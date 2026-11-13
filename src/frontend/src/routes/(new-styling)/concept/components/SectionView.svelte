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
    withAiAccess,
    withCliAccess,
    type Scenario,
  } from "../status";
  import Elevated from "./Elevated.svelte";
  import PanelHeader from "./PanelHeader.svelte";

  interface Props {
    section: Section;
    scenario: Scenario;
    signedInWith: MockIdentity["signedInWith"];
    browsers: MockBrowser[];
    onBack: () => void;
    onSignOut: (id: string) => void;
    /** Moves the scenario on, as finishing the step would. */
    onResolve: (patch: Partial<Scenario>, message: string) => void;
    /** Opens the product's phrase check; the scenario moves on once it passes. */
    onVerifyPhrase: (patch: Partial<Scenario>, message: string) => void;
    /** For the steps the concept doesn't walk through. */
    onUnavailable: (step: string) => void;
  }

  const {
    section,
    scenario,
    signedInWith,
    browsers,
    onBack,
    onSignOut,
    onResolve,
    onVerifyPhrase,
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
    access: ["Access methods", "The ways you can sign in."],
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

  const recoveryRows = $derived.by<Row[]>(() => {
    const { recovery } = scenario;
    const phrase: Row = {
      id: "phrase",
      icon: BookOpenIcon,
      title: "Recovery phrase",
      ...(recovery === "none"
        ? {
            meta: "Not set up · 24 words you write down and keep offline",
            empty: true,
            action: {
              label: "Set up",
              variant: "primary",
              onclick: () =>
                onResolve({ recovery: "noEmail" }, "Recovery phrase set up"),
            },
          }
        : recovery === "unverified"
          ? {
              meta: "Not verified yet · Check you wrote it down right",
              flag: "warning",
              action: {
                label: "Verify",
                variant: "primary",
                onclick: () =>
                  onVerifyPhrase(
                    { recovery: "noEmail" },
                    "Recovery phrase verified",
                  ),
              },
            }
          : recovery === "usedPhrase"
            ? {
                meta: "You're signed in with it right now",
                badge: { label: "Active", color: "success" },
              }
            : recovery === "phraseReset"
              ? {
                  meta: "Reset 2 hours ago",
                  badge: { label: "New" },
                  action: {
                    label: "Reset",
                    onclick: () => onUnavailable("Resetting the phrase"),
                  },
                }
              : {
                  meta: "Verified",
                  action: {
                    label: "Reset",
                    onclick: () => onUnavailable("Resetting the phrase"),
                  },
                }),
    } as Row;
    const email: Row = {
      id: "email",
      icon: MailIcon,
      title: "Recovery email",
      ...(recovery === "emailPending"
        ? {
            meta: "Waiting for your email from t•••@gmail.com",
            flag: "warning",
            action: {
              label: "See steps",
              onclick: () => onUnavailable("Email recovery steps"),
            },
          }
        : recovery === "none" ||
            recovery === "unverified" ||
            recovery === "noEmail"
          ? {
              meta: "Not set up · Send an email from your inbox to get back in",
              empty: true,
              action: {
                label: "Set up",
                onclick: () =>
                  recovery === "noEmail"
                    ? onResolve({ recovery: "calm" }, "Recovery email added")
                    : onUnavailable("Setting up a recovery email"),
              },
            }
          : {
              meta: "t•••@gmail.com",
              action: {
                label: "Replace",
                onclick: () => onUnavailable("Replacing the recovery email"),
              },
            }),
    } as Row;
    return [phrase, email];
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

<div class="flex flex-col gap-4">
  <PanelHeader title={TITLES[section][0]} lead={TITLES[section][1]} {onBack} />

  {#if section === "devices"}
    <Elevated offset={1} class="flex flex-col overflow-hidden rounded-2xl pb-1">
      {#each deviceGroups as group (group.platform)}
        <GroupHeading
          kind={kindOf(group.browsers[0].description)}
          platform={group.platform}
          count={group.browsers.length}
        />
        <ul class="ii-rows flex flex-col [--ii-rows-inset:4rem]">
          {#each group.browsers as browser (browser.id)}
            {@render row(browserRow(browser))}
          {/each}
        </ul>
      {/each}
    </Elevated>
  {:else}
    {@const rows =
      section === "access"
        ? accessRows
        : section === "recovery"
          ? recoveryRows
          : integrationRows}
    <Elevated
      offset={1}
      as="ul"
      class="ii-rows flex flex-col overflow-hidden rounded-2xl [--ii-rows-inset:4rem]"
    >
      {#each rows as item (item.id)}
        {@render row(item)}
      {/each}
    </Elevated>
    {#if section === "access"}
      <button
        type="button"
        onclick={() => onUnavailable("Adding an access method")}
        class="btn btn-secondary w-full font-medium"
      >
        <PlusIcon class="size-4" />
        Add access method
      </button>
    {/if}
  {/if}
</div>
