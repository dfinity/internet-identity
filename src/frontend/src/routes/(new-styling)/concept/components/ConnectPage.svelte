<script lang="ts">
  /**
   * Connect: using your apps from AI, which is for everyone. AI access leads
   * with its state and the switch the product's settings have, so the page is
   * the identity's own, not an ad for building; around it, how to add the
   * connector, what AI may do and which apps it used. The copy follows
   * DFINITY's ICP MCP page, told for the identity.
   *
   * The terminal is a developer's, so it waits at the bottom: one quiet line
   * until the ICP CLI first asks to sign in. Once allowed, the line becomes
   * Developer access, with its switch and each place the terminal is signed
   * in; signing the last one out folds it back into the line.
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
  const developerTitleId = `${aiTitleId}-developer`;

  const aiOn = $derived(aiAccessOn(integrations));
  const cliOn = $derived(cliAccessOn(integrations));
  const cliAsks = $derived(integrations === "cliConfirm");
  const localServer = $derived(
    integrations === "local" || integrations === "localAllowed",
  );

  /** Bumped when the concept has no state for a switch's result, so the
   *  switch is drawn again from the state and springs back. */
  let resets = $state(0);

  const MINUTE = 60 * 1000;
  const HOUR = 60 * MINUTE;
  const DAY = 24 * HOUR;
  const opened = Date.now();

  /** A place the terminal may sign in as you. */
  interface Terminal {
    id: string;
    tool: string;
    place: string;
    allowedAt: number;
    expiresAt: number;
  }

  /** Where an identity with CLI access on is signed in from the terminal. */
  const TERMINALS: Terminal[] = [
    {
      id: "macbook",
      tool: "icp-cli",
      place: "MacBook Pro",
      allowedAt: opened - 2 * HOUR,
      expiresAt: opened + 28 * DAY,
    },
    {
      id: "build-server",
      tool: "icp-cli",
      place: "build-server",
      allowedAt: opened - 5 * DAY,
      expiresAt: opened + 23 * DAY,
    },
  ];

  /** What happened on this page that the scenario has no state for: places
   *  signed out, a request just allowed, the switch turned off with places
   *  still listed. It holds while the scenario stays where the page left it;
   *  a preset or another identity starts from the scenario alone. */
  interface Local {
    at: IntegrationsState;
    signedOut: string[];
    /** When the ICP CLI's request was allowed here, so this device is the
     *  one place listed. */
    allowedAt?: number;
    switchedOff?: boolean;
  }
  let local = $state<Local>();
  const here = $derived(local?.at === integrations ? local : undefined);

  const terminals = $derived(
    (here?.allowedAt === undefined
      ? TERMINALS
      : [
          {
            ...TERMINALS[0],
            allowedAt: here.allowedAt,
            expiresAt: here.allowedAt + 30 * DAY,
          },
        ]
    ).filter(({ id }) => here?.signedOut.includes(id) !== true),
  );

  /** Developer access shows once the terminal has asked: while it waits,
   *  while it's allowed somewhere, and while switched off with places kept.
   *  Before that, and after the last place signs out, one quiet line. */
  const developer = $derived(
    cliAsks || (cliOn && terminals.length > 0) || here?.switchedOff === true,
  );

  /** Moves the scenario on and carries what this page holds with it. */
  const move = (
    next: IntegrationsState,
    message: string,
    change: Partial<Omit<Local, "at">> = {},
  ) => {
    local = { signedOut: [], ...here, ...change, at: next };
    onResolve(next === integrations ? {} : { integrations: next }, message);
  };

  const switchAi = (on: boolean) => {
    const next = withAiAccess(integrations, on);
    if (next === undefined) {
      resets += 1;
      onUnavailable("Turning AI access off while the ICP CLI waits");
      return;
    }
    move(next, on ? "AI access on" : "AI access off");
  };

  const switchCli = (on: boolean) => {
    const next = withCliAccess(integrations, on);
    if (next === undefined) {
      resets += 1;
      onUnavailable("Turning terminal sign-in off beside a local server");
      return;
    }
    move(next, on ? "Terminal sign-in on" : "Terminal sign-in off", {
      switchedOff: !on,
    });
  };

  const answerCli = (allow: boolean) =>
    move(
      withCliAccess(integrations, allow) ?? integrations,
      allow ? "ICP CLI allowed" : "ICP CLI declined",
      allow ? { signedOut: [], allowedAt: Date.now() } : {},
    );

  /** Signs one place out. The last one takes CLI access with it, where the
   *  concept has a state for that, so the section folds into the line. */
  const signOut = ({ id, tool, place }: Terminal) => {
    const last = terminals.length === 1;
    move(
      last
        ? (withCliAccess(integrations, false) ?? integrations)
        : integrations,
      `${tool} signed out on ${place}`,
      { signedOut: [...(here?.signedOut ?? []), id] },
    );
  };

  const allowedFor = ({ allowedAt, expiresAt }: Terminal) =>
    `Allowed ${
      Date.now() - allowedAt < MINUTE
        ? "just now"
        : $formatRelative(new Date(allowedAt), { style: "long" })
    } · expires ${$formatRelative(new Date(expiresAt), { style: "long" })}`;

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
  /** The ICP CLI's official source, as the product's CLI dialog links it. */
  const CLI_DOCS = "https://github.com/dfinity/icp-cli";

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

<!-- The ICP CLI's docs, from the developer's line and from its section. -->
{#snippet docsLink()}
  <a
    href={CLI_DOCS}
    target="_blank"
    rel="noopener noreferrer"
    class={["shrink-0", TEXT_LINK]}
  >
    Docs<span class="sr-only">&nbsp;for the ICP CLI</span>
    <ArrowUpRightIcon class="size-3.5 rtl:-scale-x-100" aria-hidden="true" />
  </a>
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
      Use your apps from AI
    </h2>
    <p
      class="text-text-tertiary max-w-[60ch] text-[15px] leading-6 text-pretty"
    >
      Internet Identity signs your assistant in to your apps as you, with only
      the access you allow, for as long as you allow it.
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
                  onclick: () => move("calm", "Local server removed"),
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

    <!-- What AI may do once signed in, and where it stops: the whole row,
         now that AI's two tiles above have it to themselves. -->
    <section
      class={["flex flex-col gap-5 @2xl:col-span-2 @4xl:col-span-6", TILE]}
    >
      <div class="flex flex-col gap-1">
        <h3 class={TILE_TITLE}>What AI can do as you</h3>
        <p class={TILE_BODY}>
          Only for as long as you allowed. Remove the connector and it stops at
          once.
        </p>
      </div>
      <ul class="grid gap-x-4 gap-y-5 @2xl:grid-cols-2 @4xl:grid-cols-4">
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
  </div>

  {#if developer}
    <!-- Developer access: the terminal, once it has asked. A section of its
         own under AI, so the developer's tool never reads as everyone's. -->
    <section aria-labelledby={developerTitleId} class="flex flex-col gap-3">
      <div class="flex items-center justify-between gap-3">
        <h2
          id={developerTitleId}
          class="text-text-primary text-xl font-semibold tracking-tight"
        >
          Developer access
        </h2>
        {@render docsLink()}
      </div>
      <div class={["flex flex-col gap-4", TILE]}>
        {#if cliAsks}
          <!-- The ICP CLI asked from this device: the request is the whole
               card until it's answered, with no switch to turn on yet. -->
          <div
            class="grid grid-cols-[auto_1fr] items-center gap-x-4 gap-y-3 @2xl:grid-cols-[auto_1fr_auto]"
          >
            <span aria-hidden="true" class={ICON_CHIP}>
              <SquareTerminalIcon class="size-5" />
            </span>
            <span class="flex min-w-0 flex-col gap-0.5">
              <h3 id={cliTitleId} class={TILE_TITLE}>
                The ICP CLI asks to sign in as you
              </h3>
              <span
                class="text-warning-700 dark:text-text-warning-primary text-[13px] leading-[18px]"
              >
                On this device · Waiting for you
              </span>
            </span>
            <span class="col-start-2 flex shrink-0 gap-2 @2xl:col-start-auto">
              <button
                type="button"
                onclick={() => answerCli(false)}
                class="btn btn-secondary btn-sm font-medium"
              >
                Decline
              </button>
              <button
                type="button"
                onclick={() => answerCli(true)}
                class="btn btn-primary btn-sm font-medium"
              >
                Allow
              </button>
            </span>
          </div>
          <div class="flex flex-col gap-2">
            <p class={FIELD_LABEL}>Only allow the official ICP CLI</p>
            {@render copyField(
              "cli-install",
              CLI_INSTALL,
              "the install command",
              true,
            )}
          </div>
        {:else}
          {@render accessHead(
            cliTitleId,
            SquareTerminalIcon,
            "Sign in from the terminal",
            undefined,
            cliOn,
            switchCli,
          )}
          <p class={TILE_BODY}>
            Tools you run can sign in to your apps as you.
          </p>
          {#if cliOn}
            <div class="flex flex-col gap-2">
              <p class={FIELD_LABEL}>Allowed to sign in</p>
              <ul class={["ii-rows flex flex-col overflow-hidden", PLATE]}>
                {#each terminals as terminal (terminal.id)}
                  <li
                    class="flex flex-wrap items-center gap-x-3 gap-y-2 px-4 py-3"
                  >
                    <span class="flex min-w-0 flex-1 flex-col gap-0.5">
                      <span
                        class="text-text-primary truncate text-sm font-semibold"
                      >
                        {terminal.tool} · {terminal.place}
                      </span>
                      <span
                        class="text-text-tertiary text-[13px] leading-[18px] text-pretty"
                      >
                        {allowedFor(terminal)}
                      </span>
                    </span>
                    <button
                      type="button"
                      onclick={() => signOut(terminal)}
                      class="btn btn-secondary btn-sm shrink-0 font-medium"
                    >
                      Sign out
                    </button>
                  </li>
                {/each}
              </ul>
            </div>
          {:else}
            <p
              class="border-border-secondary text-text-tertiary rounded-2xl border border-dashed px-4 py-3.5 text-[13px] leading-[18px] text-pretty"
            >
              Off. Tools you run can't sign in as you until you switch it back
              on.
            </p>
          {/if}
        {/if}
      </div>
    </section>
  {:else}
    <!-- Before the terminal ever asked, one quiet line for whoever builds:
         nothing to switch on until the CLI asks here first. -->
    <p
      class="text-text-tertiary flex items-start gap-2.5 text-[13px] leading-5"
    >
      <SquareTerminalIcon
        class="text-fg-quaternary mt-0.5 size-4 shrink-0"
        aria-hidden="true"
      />
      <span class="min-w-0 text-pretty">
        Building on ICP? Run <code
          class="text-text-primary rounded-md bg-(--ii-selected) px-1.5 py-0.5 font-mono text-xs whitespace-nowrap"
          >icp identity login</code
        > in your terminal. We'll ask for your OK here first.
      </span>
      {@render docsLink()}
    </p>
  {/if}

  <!-- Copying says so on the button; this says it to a screen reader. -->
  <span class="sr-only" aria-live="polite">
    {copied !== undefined ? "Copied" : ""}
  </span>
</div>
