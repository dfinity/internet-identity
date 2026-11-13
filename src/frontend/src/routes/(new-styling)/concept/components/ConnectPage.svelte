<script lang="ts">
  /**
   * Connect: what can use your apps as you outside the browser. An AI
   * assistant, through the Internet Computer MCP, and command-line tools on
   * this device. Each leads with its state and the switch the product's
   * settings have, so the page is the identity's own, not an ad for building;
   * around them, how to add the connector, what AI may do and which apps it
   * used. The copy follows DFINITY's ICP MCP page, told for the identity.
   */
  import type { Component } from "svelte";
  import {
    AppWindowIcon,
    ArrowUpRightIcon,
    BotIcon,
    CheckIcon,
    CopyIcon,
    PencilLineIcon,
    SearchIcon,
    ServerIcon,
    SlidersHorizontalIcon,
    SquareTerminalIcon,
    UserRoundCheckIcon,
  } from "@lucide/svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { formatRelative } from "$lib/stores/locale.store";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { ConceptApp } from "../mockData";
  import {
    aiAccessOn,
    cliAccessOn,
    withAiAccess,
    withCliAccess,
    type IntegrationsState,
    type Scenario,
  } from "../status";

  interface Props {
    integrations: IntegrationsState;
    apps: ConceptApp[];
    /** Moves the scenario on, as the switch or the step would. */
    onResolve: (patch: Partial<Scenario>, message: string) => void;
    /** For the steps the concept doesn't walk through. */
    onUnavailable: (step: string) => void;
  }

  const { integrations, apps, onResolve, onUnavailable }: Props = $props();
  const aiTitleId = $props.id();
  const cliTitleId = `${aiTitleId}-cli`;

  const aiOn = $derived(aiAccessOn(integrations));
  const cliOn = $derived(cliAccessOn(integrations));
  const localServer = $derived(
    integrations === "local" || integrations === "localAllowed",
  );

  /** Bumped when the concept has no state for a switch's result, so the
   *  switch is drawn again from the state and springs back. */
  let resets = $state(0);

  const switchAi = (on: boolean) => {
    const next = withAiAccess(integrations, on);
    if (next === undefined) {
      resets += 1;
      onUnavailable("Turning AI access off while the ICP CLI waits");
      return;
    }
    onResolve({ integrations: next }, on ? "AI access on" : "AI access off");
  };

  const switchCli = (on: boolean) => {
    const next = withCliAccess(integrations, on);
    if (next === undefined) {
      resets += 1;
      onUnavailable("Turning CLI access off beside a local server");
      return;
    }
    onResolve({ integrations: next }, on ? "CLI access on" : "CLI access off");
  };

  const switched = (event: Event) =>
    event.currentTarget instanceof HTMLInputElement &&
    event.currentTarget.checked;

  /** The apps AI signed in to as this identity, latest first. */
  const usedByAi = $derived(
    apps
      .flatMap((app) =>
        app.aiUsedMillis === undefined ? [] : [{ app, at: app.aiUsedMillis }],
      )
      .sort((a, b) => b.at - a.at),
  );

  const MCP_URL = "https://mcp.internetcomputer.org/mcp";
  /** A URL in pieces that end at a slash or a dot, where it may wrap. */
  const urlParts = (url: string) => url.split(/(?<=[/.])(?!\/)/);
  const CLI_INSTALL = "npm install -g @icp-sdk/icp-cli";

  const STEPS: { title: string; body: string }[] = [
    {
      title: "Add the connector",
      body: "ChatGPT has an ICP plugin. In Claude, Gemini, Perplexity and others, add this URL as a custom connector.",
    },
    {
      title: "Sign in with Internet Identity",
      body: "Choose the identity it acts as, and how long the session lasts.",
    },
    {
      title: "Choose what it may do",
      body: "Actions and questions, or questions only.",
    },
  ];

  /** What AI may do once you've signed it in, from the ICP MCP page. */
  const CAN: { icon: Component; title: string; body: string }[] = [
    {
      icon: AppWindowIcon,
      title: "Opens your apps",
      body: "By name or link, the way you would.",
    },
    {
      icon: UserRoundCheckIcon,
      title: "Signs in as you",
      body: "Internet Identity signs it in to each app, for the session you allowed.",
    },
    {
      icon: SearchIcon,
      title: "Looks things up",
      body: "It reads what an app shares about itself: its interface, docs and answers.",
    },
    {
      icon: PencilLineIcon,
      title: "Changes only what's allowed",
      body: "Only in apps that opted in, and only if you chose actions.",
    },
  ];

  /** The field just copied, so its button says so for a moment. */
  let copied = $state<string>();
  let copiedTimer: ReturnType<typeof setTimeout> | undefined;

  const copy = async (id: string, text: string) => {
    try {
      await navigator.clipboard.writeText(text);
    } catch {
      toaster.create({ title: "Couldn't copy it", duration: 3000 });
      return;
    }
    copied = id;
    clearTimeout(copiedTimer);
    copiedTimer = setTimeout(() => (copied = undefined), 1600);
  };

  const TILE = "ii-shelf rounded-[1.75rem] p-6 @2xl:p-7";
  const FOCUS =
    "focus-visible:outline-focus-ring focus-visible:outline-2 focus-visible:outline-offset-2";
  const TILE_TITLE =
    "text-text-primary text-lg leading-6 font-semibold tracking-tight text-balance";
  const TILE_BODY = "text-text-tertiary text-sm text-pretty";
  const ICON_CHIP =
    "text-fg-secondary grid size-11 shrink-0 place-items-center rounded-xl bg-(--ii-selected)";
  const FIELD_LABEL = "text-text-tertiary text-xs font-medium";
  /** A row raised off its shelf, as the product's connector card is. */
  const PLATE = "rounded-2xl bg-(--ii-surface-3) shadow-(--ii-shadow-1)";
  const TEXT_LINK = [
    "text-text-secondary hover:text-text-primary inline-flex w-fit items-center gap-1 rounded-sm text-sm font-medium transition-colors duration-150",
    FOCUS,
  ];
</script>

<!-- A URL or a command, with a button that copies it. A command wraps only
     between its words, never at a hyphen inside a flag or a package; a URL
     only after a slash or a dot. -->
{#snippet copyField(id: string, text: string, what: string, shell = false)}
  <div class={["flex items-start gap-2 py-1 ps-3 pe-1", PLATE, "rounded-xl"]}>
    <code
      class="text-text-primary min-w-0 flex-1 py-1.5 font-mono text-xs leading-5"
    >
      {#if shell}<span class="text-fg-quaternary select-none" aria-hidden="true"
          >$&nbsp;</span
        >{#each text.split(" ") as word, at (at)}{at > 0 ? " " : ""}<span
            class="whitespace-nowrap">{word}</span
          >{/each}{:else}{#each urlParts(text) as part, at (at)}{#if at > 0}<wbr
            />{/if}{part}{/each}{/if}
    </code>
    <button
      type="button"
      onclick={() => copy(id, text)}
      aria-label={`Copy ${what}`}
      class={[
        "text-text-secondary inline-flex h-7 min-w-[5.25rem] shrink-0 items-center justify-center gap-1.5 rounded-lg px-2 text-xs font-medium transition-colors duration-150 hover:bg-(--ii-hover) active:bg-(--ii-selected)",
        FOCUS,
      ]}
    >
      {#if copied === id}
        <CheckIcon
          class="text-fg-success-primary size-3.5"
          aria-hidden="true"
        />
        Copied
      {:else}
        <CopyIcon class="size-3.5" aria-hidden="true" />
        Copy
      {/if}
    </button>
  </div>
{/snippet}

<!-- An access's head: its icon, its name and state, and its switch. -->
{#snippet accessHead(
  titleId: string,
  icon: Component,
  title: string,
  badge: string | undefined,
  checked: boolean,
  onswitch: (on: boolean) => void,
)}
  {@const Icon = icon}
  <div class="flex items-center gap-4">
    <span aria-hidden="true" class={ICON_CHIP}>
      <Icon class="size-5" />
    </span>
    <div class="flex min-w-0 flex-1 flex-wrap items-center gap-x-2 gap-y-1">
      <h3 id={titleId} class={TILE_TITLE}>{title}</h3>
      {#if badge !== undefined}
        <Badge color="success" size="sm" dot>{badge}</Badge>
      {/if}
    </div>
    <!-- Relative, so the switch's hidden input sits in the toggle rather
         than off on the canvas: focusing it there scrolled the canvas. -->
    {#key resets}
      <Toggle
        class="relative"
        {checked}
        onchange={(event) => onswitch(switched(event))}
        aria-labelledby={titleId}
      />
    {/key}
  </div>
{/snippet}

<!-- A connector AI access trusts, as the product's settings list it. -->
{#snippet connector(
  icon: Component,
  name: string,
  meta: string,
  url: string | undefined,
  risk: boolean,
  action: { label: string; danger?: boolean; onclick: () => void },
)}
  {@const Icon = icon}
  <!-- On a phone the button goes under the text, so the name keeps its
       width. -->
  <li
    class="grid grid-cols-[auto_1fr] items-center gap-x-3 gap-y-2 px-4 py-3 @2xl:grid-cols-[auto_1fr_auto]"
  >
    <span
      aria-hidden="true"
      class="text-fg-secondary grid size-10 shrink-0 place-items-center rounded-xl bg-(--ii-selected)"
    >
      <Icon class="size-4.5" />
    </span>
    <span class="flex min-w-0 flex-1 flex-col gap-0.5">
      <span class="text-text-primary truncate text-sm font-semibold">
        {name}
      </span>
      <span
        class={[
          "text-[13px] leading-[18px] text-pretty",
          risk ? "text-text-error-primary" : "text-text-tertiary",
        ]}
      >
        {meta}
      </span>
      {#if url !== undefined}
        <span class="text-text-tertiary truncate font-mono text-xs">{url}</span>
      {/if}
    </span>
    <button
      type="button"
      onclick={action.onclick}
      class={[
        "btn btn-secondary btn-sm col-start-2 shrink-0 gap-2 justify-self-start font-medium @2xl:col-start-auto",
        action.danger === true && "btn-danger",
      ]}
    >
      {#if action.danger !== true}
        <SlidersHorizontalIcon class="size-4" aria-hidden="true" />
      {/if}
      {action.label}
    </button>
  </li>
{/snippet}

<div class="mx-auto flex w-full max-w-5xl flex-col gap-8 px-2">
  <header class="flex flex-col gap-1">
    <h2
      class="text-text-primary text-2xl leading-8 font-semibold tracking-tight"
    >
      Use your apps from AI and the terminal
    </h2>
    <p
      class="text-text-tertiary max-w-[60ch] text-[15px] leading-6 text-pretty"
    >
      Internet Identity signs them in to your apps as you, with only the access
      you allow, for as long as you allow it.
    </p>
  </header>

  <div class="grid grid-cols-1 gap-3 @2xl:grid-cols-2 @4xl:grid-cols-6">
    <!-- AI access: the state first, then the connector it trusts. -->
    <section
      aria-labelledby={aiTitleId}
      class={["flex flex-col gap-4 @2xl:col-span-2 @4xl:col-span-3", TILE]}
    >
      {@render accessHead(
        aiTitleId,
        BotIcon,
        "AI access",
        aiOn ? "Enabled on all devices" : undefined,
        aiOn,
        switchAi,
      )}
      <p class={TILE_BODY}>
        Ask questions and act across your apps by chatting with AI. It reaches
        them through the Internet Computer MCP, the official connector by
        DFINITY.
      </p>
      {#if aiOn}
        <div class="flex flex-col gap-2">
          <p class={FIELD_LABEL}>
            {localServer ? "Trusted connectors" : "Trusted connector"}
          </p>
          <ul
            class={[
              "ii-rows flex flex-col overflow-hidden [--ii-rows-inset:4.25rem]",
              PLATE,
            ]}
          >
            {@render connector(
              McpIcon,
              "Internet Computer MCP",
              "Official · Hosted by DFINITY",
              MCP_URL,
              false,
              {
                label: "Customize",
                onclick: () => onUnavailable("Customizing the connector"),
              },
            )}
            {#if localServer}
              {@render connector(
                ServerIcon,
                "Local server",
                "Runs on this computer · Any local program can connect",
                undefined,
                integrations === "local",
                {
                  label: "Remove",
                  danger: true,
                  onclick: () =>
                    onResolve({ integrations: "calm" }, "Local server removed"),
                },
              )}
            {/if}
          </ul>
        </div>
      {:else}
        <p
          class="border-border-secondary text-text-tertiary rounded-2xl border border-dashed px-4 py-3.5 text-[13px] leading-[18px] text-pretty"
        >
          Off. Switch it on to trust the official connector, then add it to your
          assistant.
        </p>
      {/if}
      <!-- The apps AI used as you: what the identity can show that the
           assistant's own history doesn't. -->
      {#if usedByAi.length > 0}
        <div class="flex flex-col gap-2">
          <p class={FIELD_LABEL}>Lately used by AI as you</p>
          <ul
            class={[
              "ii-rows flex flex-col overflow-hidden [--ii-rows-inset:4.5rem]",
              PLATE,
            ]}
          >
            {#each usedByAi as { app, at } (app.origin)}
              <li class="flex items-center gap-3 px-4 py-2.5">
                <AppLogo logo={app.logo} size="card" />
                <span
                  class="text-text-primary min-w-0 flex-1 truncate text-sm font-medium"
                >
                  {app.name}
                </span>
                <span class="text-text-tertiary shrink-0 text-xs">
                  {$formatRelative(new Date(at), { style: "long" })}
                </span>
              </li>
            {/each}
          </ul>
        </div>
      {/if}
    </section>

    <!-- How to add it: three steps, and the URL to paste. -->
    <section
      class={["flex flex-col gap-4 @2xl:col-span-2 @4xl:col-span-3", TILE]}
    >
      <h3 class={TILE_TITLE}>Add it to your assistant</h3>
      <ol class="flex flex-col gap-3">
        {#each STEPS as step, index (step.title)}
          <li class="flex gap-3">
            <span
              aria-hidden="true"
              class="text-text-secondary grid size-6 shrink-0 place-items-center rounded-full bg-(--ii-selected) text-xs font-semibold tabular-nums"
            >
              {index + 1}
            </span>
            <span class="flex min-w-0 flex-col gap-0.5">
              <span class="text-text-primary text-sm font-medium">
                {step.title}
              </span>
              <span
                class="text-text-tertiary text-[13px] leading-[18px] text-pretty"
              >
                {step.body}
              </span>
            </span>
          </li>
        {/each}
      </ol>
      {@render copyField("mcp-url", MCP_URL, "the connector URL")}
      <a
        href="https://internetcomputer.org/icp-mcp/"
        target="_blank"
        rel="noopener noreferrer"
        class={["pt-1", TEXT_LINK]}
      >
        About ICP MCP
        <ArrowUpRightIcon
          class="size-3.5 rtl:-scale-x-100"
          aria-hidden="true"
        />
      </a>
    </section>

    <!-- What AI may do once signed in, and where it stops. -->
    <section
      class={["flex flex-col gap-5 @2xl:col-span-2 @4xl:col-span-3", TILE]}
    >
      <div class="flex flex-col gap-1">
        <h3 class={TILE_TITLE}>What AI can do as you</h3>
        <p class={TILE_BODY}>
          Only for as long as you allowed. Remove the connector and it stops at
          once.
        </p>
      </div>
      <ul class="grid gap-x-4 gap-y-5 @2xl:grid-cols-2">
        {#each CAN as item (item.title)}
          {@const Icon = item.icon}
          <li class="flex flex-col gap-0.5">
            <span class="flex items-center gap-2">
              <Icon
                class="text-fg-secondary size-4 shrink-0"
                aria-hidden="true"
              />
              <span class="text-text-primary text-sm font-medium">
                {item.title}
              </span>
            </span>
            <span
              class="text-text-tertiary text-[13px] leading-[18px] text-pretty"
            >
              {item.body}
            </span>
          </li>
        {/each}
      </ul>
    </section>

    <!-- CLI access: per device, with a CLI that waits for approval. -->
    <section
      aria-labelledby={cliTitleId}
      class={["flex flex-col gap-4 @2xl:col-span-2 @4xl:col-span-3", TILE]}
    >
      {@render accessHead(
        cliTitleId,
        SquareTerminalIcon,
        "CLI access",
        cliOn ? "Enabled on this device" : undefined,
        cliOn,
        switchCli,
      )}
      <p class={TILE_BODY}>
        {cliOn
          ? "Command-line tools on this device can ask to sign you in to apps."
          : "Let command-line tools on this device sign in to apps using your identity."}
      </p>
      {#if integrations === "cliConfirm"}
        <div
          class={[
            "flex flex-wrap items-center gap-x-3 gap-y-2 px-4 py-3",
            PLATE,
          ]}
        >
          <span class="flex min-w-0 flex-1 flex-col gap-0.5">
            <span class="text-text-primary text-sm font-semibold">
              The ICP CLI asks to sign in as you
            </span>
            <span
              class="text-warning-700 dark:text-text-warning-primary text-[13px] leading-[18px]"
            >
              On this device · Waiting for you
            </span>
          </span>
          <span class="flex shrink-0 gap-2">
            <button
              type="button"
              onclick={() =>
                onResolve({ integrations: "aiOnly" }, "ICP CLI declined")}
              class="btn btn-secondary btn-sm font-medium"
            >
              Decline
            </button>
            <button
              type="button"
              onclick={() =>
                onResolve({ integrations: "calm" }, "ICP CLI approved")}
              class="btn btn-primary btn-sm font-medium"
            >
              Approve
            </button>
          </span>
        </div>
      {/if}
      <div class="mt-auto flex flex-col gap-2">
        <p class={FIELD_LABEL}>Only for the official ICP CLI</p>
        {@render copyField(
          "cli-install",
          CLI_INSTALL,
          "the install command",
          true,
        )}
      </div>
    </section>
  </div>

  <!-- Copying says so on the button; this says it to a screen reader. -->
  <span class="sr-only" aria-live="polite">
    {copied !== undefined ? "Copied" : ""}
  </span>
</div>
