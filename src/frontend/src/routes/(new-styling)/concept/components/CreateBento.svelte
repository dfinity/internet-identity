<script lang="ts">
  /**
   * Create: the ways to make an app of your own on the Internet Computer, as
   * a bento grid. Room follows reach: Caffeine, which needs nothing but
   * words, gets the most; what every app here gets sits beside it; the ways
   * that take code come next; Open Cloud shows how far it goes; docs close
   * the page. Tiles lie flat on shelves, as a shadow on the canvas means
   * something waits for you. Every link leaves for the real site.
   */
  import type { Component, Snippet } from "svelte";
  import {
    ArrowUpIcon,
    ArrowUpRightIcon,
    CheckIcon,
    CodeXmlIcon,
    CopyIcon,
    DatabaseIcon,
    HandCoinsIcon,
    ServerOffIcon,
    ShieldCheckIcon,
    SquareTerminalIcon,
  } from "@lucide/svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import { createApps } from "../mockData";

  const { caffeine, openCloud, juno } = createApps;

  /** What every app on the Internet Computer gets, whichever way it's built. */
  const WHY: { icon: Component; title: string; body: string }[] = [
    {
      icon: ShieldCheckIcon,
      title: "Tamperproof",
      body: "Your app runs on many independent machines that must agree on every change.",
    },
    {
      icon: ServerOffIcon,
      title: "Nothing to switch off",
      body: "No servers or cloud account. The network itself serves your app to the web.",
    },
    {
      icon: DatabaseIcon,
      title: "The program is the database",
      body: "Data lives inside your app and survives every update.",
    },
    {
      icon: HandCoinsIcon,
      title: "Free for the people who use it",
      body: "Your app pays for its own compute, so no one needs a wallet or tokens.",
    },
  ];

  // From DFINITY's guide to coding agents and the ICP MCP page.
  const SKILLS_PROMPT =
    "Fetch https://skills.internetcomputer.org/llms.txt and follow its instructions when building on ICP";
  const SKILLS_PIN = "npx skills add dfinity/icskills";
  const MCP_URL = "https://mcp.internetcomputer.org/mcp";
  const MCP_CLAUDE_CODE = `claude mcp add --transport http icp ${MCP_URL}`;
  // The icp CLI's quickstart, a Motoko backend with a React frontend.
  const CLI_COMMANDS = [
    "npm install -g @icp-sdk/icp-cli @icp-sdk/ic-wasm",
    "icp new hello-icp --subfolder hello-world --silent",
    "cd hello-icp",
    "icp network start -d",
    "icp deploy",
  ].join("\n");

  type AgentWay = "skills" | "mcp";
  const AGENT_WAYS: { value: AgentWay; label: string }[] = [
    { value: "skills", label: "Skills" },
    { value: "mcp", label: "MCP" },
  ];
  let agentWay = $state<AgentWay>("skills");

  /** An engine across seven providers with one of them down, as Open Cloud
   *  shows its own. Illustrative. */
  const NODES: { code: string; down?: boolean }[] = [
    { code: "CH" },
    { code: "DE" },
    { code: "US" },
    { code: "BR", down: true },
    { code: "JP" },
    { code: "SG" },
    { code: "AU" },
  ];

  const DOCS: { title: string; detail: string; url: string }[] = [
    {
      title: "Quickstart",
      detail: "A full-stack app, deployed in five minutes",
      url: "https://docs.internetcomputer.org/getting-started/quickstart/",
    },
    {
      title: "Coding with AI agents",
      detail: "Set up Claude Code, Cursor or Copilot for ICP",
      url: "https://docs.internetcomputer.org/guides/ai-coding-agents/",
    },
    {
      title: "Host a website",
      detail: "Serve a site straight from the network",
      url: "https://docs.internetcomputer.org/guides/frontends/static-site/overview/",
    },
    {
      title: "Keep data across updates",
      detail: "How your app's state outlives an upgrade",
      url: "https://docs.internetcomputer.org/guides/backends/data-persistence/",
    },
    {
      title: "Bitcoin, Ethereum, Solana",
      detail: "Use other chains without a bridge",
      url: "https://docs.internetcomputer.org/guides/chain-fusion/bitcoin/",
    },
    {
      title: "Hand it to a DAO",
      detail: "Let a community govern your app",
      url: "https://docs.internetcomputer.org/guides/governance/launching/",
    },
    {
      title: "Motoko",
      detail: "The language made for the Internet Computer",
      url: "https://docs.internetcomputer.org/languages/motoko/",
    },
    {
      title: "Rust",
      detail: "Write your app's backend in Rust",
      url: "https://docs.internetcomputer.org/languages/rust/",
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

  /** Caffeine's tile, in Caffeine's near-black. */
  const CAFFEINE_COLOR = "#0c0b0b";

  const TILE = "rounded-[1.75rem] p-6 @2xl:p-7";
  const FOCUS =
    "focus-visible:outline-focus-ring focus-visible:outline-2 focus-visible:outline-offset-2";
  const TILE_TITLE =
    "text-text-primary text-lg leading-6 font-semibold tracking-tight text-balance";
  const TILE_BODY = "text-text-tertiary text-sm text-pretty";
  const ICON_CHIP =
    "text-fg-secondary grid size-11 shrink-0 place-items-center rounded-xl bg-(--ii-selected)";
  const FIELD_LABEL = "text-text-tertiary text-xs font-medium";
  const TEXT_LINK = [
    "text-text-secondary hover:text-text-primary inline-flex w-fit items-center gap-1 rounded-sm text-sm font-medium transition-colors duration-150",
    FOCUS,
  ];
</script>

<!-- A prompt, a URL or commands, with a button that copies them. A command
     wraps only between its words, never at a hyphen inside a flag or a
     package, and a wrapped one hangs under its own `$`. A block of several
     commands puts the button above them, so they get the whole width. -->
{#snippet copyButton(id: string, text: string, what: string)}
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
      <CheckIcon class="text-fg-success-primary size-3.5" aria-hidden="true" />
      Copied
    {:else}
      <CopyIcon class="size-3.5" aria-hidden="true" />
      Copy
    {/if}
  </button>
{/snippet}

{#snippet codeLines(text: string, shell: boolean)}
  {#each text.split("\n") as line, index (index)}
    <span class={["block", shell && "ps-3.5 -indent-3.5"]}>
      {#if shell}<span class="text-fg-quaternary select-none" aria-hidden="true"
          >$&nbsp;</span
        >{/if}{#each line.split(" ") as word, at (at)}{at > 0 ? " " : ""}<span
          class={word.includes("://")
            ? "[overflow-wrap:anywhere]"
            : "whitespace-nowrap"}>{word}</span
        >{/each}
    </span>
  {/each}
{/snippet}

{#snippet copyField(id: string, text: string, what: string, shell = false)}
  {#if text.includes("\n")}
    <div class="rounded-xl bg-(--ii-surface-3) shadow-(--ii-shadow-1)">
      <div class="flex items-center justify-between gap-2 ps-3 pe-1 pt-1">
        <span class="text-text-tertiary text-xs font-medium">Terminal</span>
        {@render copyButton(id, text, what)}
      </div>
      <code
        class="text-text-primary block px-3 pt-1 pb-3 font-mono text-xs leading-5"
      >
        {@render codeLines(text, shell)}
      </code>
    </div>
  {:else}
    <div
      class="flex items-start gap-2 rounded-xl bg-(--ii-surface-3) py-1 ps-3 pe-1 shadow-(--ii-shadow-1)"
    >
      <code
        class="text-text-primary min-w-0 flex-1 py-1.5 font-mono text-xs leading-5"
      >
        {@render codeLines(text, shell)}
      </code>
      {@render copyButton(id, text, what)}
    </div>
  {/if}
{/snippet}

<!-- A tile that is one link: its icon, where it goes, and the arrow that
     says it leaves this page. -->
{#snippet linkTile(
  href: string,
  host: string,
  title: string,
  body: string,
  icon: Snippet<[]>,
  span: string,
)}
  <a
    {href}
    target="_blank"
    rel="noopener noreferrer"
    class={["ii-shelf ii-hover group flex flex-col gap-4", TILE, FOCUS, span]}
  >
    <div class="flex items-start justify-between">
      {@render icon()}
      <ArrowUpRightIcon
        class="text-fg-quaternary group-hover:text-fg-secondary size-5 transition-[color,translate] duration-200 ease-(--ii-ease-out) group-hover:translate-x-0.5 group-hover:-translate-y-0.5 motion-reduce:transition-none rtl:-scale-x-100"
        aria-hidden="true"
      />
    </div>
    <div class="flex flex-col gap-1.5">
      <h3 class={TILE_TITLE}>{title}</h3>
      <p class={TILE_BODY}>{body}</p>
    </div>
    <span class="text-text-tertiary mt-auto pt-2 text-xs font-medium">
      {host}
    </span>
  </a>
{/snippet}

<div class="mx-auto flex w-full max-w-5xl flex-col gap-8 px-2">
  <header class="flex flex-col gap-1">
    <h2
      class="text-text-primary text-2xl leading-8 font-semibold tracking-tight"
    >
      Build on the Internet Computer
    </h2>
    <p class="text-text-tertiary text-[15px] leading-6 text-pretty">
      Tamperproof apps, on a network no one can switch off.
    </p>
  </header>

  <div class="grid grid-cols-1 gap-3 @2xl:grid-cols-2 @4xl:grid-cols-6">
    <!-- Caffeine: the way that needs nothing but words, so the most room. -->
    <a
      href={caffeine.url}
      target="_blank"
      rel="noopener noreferrer"
      style:background-color={CAFFEINE_COLOR}
      class={[
        "group relative isolate flex flex-col overflow-hidden text-white @2xl:col-span-2 @4xl:col-span-4 @4xl:row-span-2",
        TILE,
        FOCUS,
        "@2xl:p-9",
      ]}
    >
      <h3
        class="max-w-[16ch] text-[1.75rem] leading-[1.1] font-semibold tracking-tight text-balance @2xl:text-[2.5rem]"
      >
        Say what you want. Get a live app.
      </h3>
      <p class="mt-3 text-sm text-pretty opacity-75">
        Chat with Caffeine. It writes the app and puts it online.
      </p>

      <!-- The art: a prompt as Caffeine takes one, in one line. -->
      <div
        aria-hidden="true"
        class="mt-6 flex items-center gap-3 rounded-3xl bg-white/[0.07] py-2 ps-4 pe-2 ring-1 ring-white/10 transition-transform duration-500 ease-(--ii-ease-out) group-hover:-translate-y-1 motion-reduce:transition-none @2xl:ps-5"
      >
        <!-- The caret stays with the last word, never on a line of its own. -->
        <p
          class="min-w-0 flex-1 text-sm leading-6 text-white/90 @2xl:text-[15px]"
        >
          A booking page for my yoga <span class="whitespace-nowrap"
            >studio<span class="caret"></span></span
          >
        </p>
        <span
          class="grid size-8 shrink-0 place-items-center rounded-full bg-white text-neutral-950"
        >
          <ArrowUpIcon class="size-4" />
        </span>
      </div>

      <div class="mt-auto flex items-center gap-3 pt-8">
        <AppLogo logo={caffeine.logo} size="lg" />
        <span class="flex min-w-0 flex-col">
          <span class="text-sm font-semibold">{caffeine.name}</span>
          <span class="text-xs opacity-70">caffeine.ai</span>
        </span>
        <span
          aria-hidden="true"
          class="ms-auto shrink-0 rounded-full bg-white px-4 py-1.5 text-sm font-semibold text-neutral-950"
        >
          Start building
        </span>
      </div>
    </a>

    <!-- What every app gets: the part to stress, whichever way it's built. -->
    <section
      class={[
        "ii-shelf flex flex-col gap-4 @2xl:col-span-2 @4xl:row-span-2",
        TILE,
      ]}
    >
      <div class="flex flex-col gap-1">
        <h3 class={TILE_TITLE}>What every app here gets</h3>
        <p class={TILE_BODY}>Whichever way you build it.</p>
      </div>
      <!-- Each icon sits in its title's line, so the text below gets the
           tile's whole width and the tile stays as low as Caffeine's. -->
      <ul class="grid gap-4 @2xl:grid-cols-2 @4xl:grid-cols-1">
        {#each WHY as item (item.title)}
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

    <!-- Your own agent: skills teach it, MCP lets it act. One copy each. -->
    <section class={["ii-shelf flex flex-col gap-4 @4xl:col-span-3", TILE]}>
      <span aria-hidden="true" class={ICON_CHIP}>
        <McpIcon class="size-5" />
      </span>
      <div class="flex flex-col gap-1.5">
        <h3 class={TILE_TITLE}>Build with your own AI agent</h3>
        <p class={TILE_BODY}>
          Claude Code, Cursor, Codex and others learn the Internet Computer from
          DFINITY's skills, and work with its apps through MCP.
        </p>
      </div>
      <div
        role="group"
        aria-label="How your agent connects"
        class="flex w-fit gap-0.5 rounded-full bg-(--ii-selected) p-0.5"
      >
        {#each AGENT_WAYS as way (way.value)}
          <button
            type="button"
            aria-pressed={agentWay === way.value}
            onclick={() => (agentWay = way.value)}
            class={[
              "rounded-full px-3.5 py-1 text-xs font-semibold transition-colors duration-150 motion-reduce:transition-none",
              FOCUS,
              agentWay === way.value
                ? "text-text-primary bg-(--ii-surface-5) shadow-(--ii-shadow-2)"
                : "text-text-tertiary hover:text-text-secondary",
            ]}
          >
            {way.label}
          </button>
        {/each}
      </div>
      <!-- Both panes share one cell, so switching never moves the grid. -->
      <div class="grid">
        {#each AGENT_WAYS as way (way.value)}
          <div
            inert={agentWay !== way.value}
            class={[
              "col-start-1 row-start-1 flex flex-col gap-3 transition-[opacity,visibility] duration-150 motion-reduce:transition-none",
              agentWay !== way.value && "invisible opacity-0",
            ]}
          >
            {#if way.value === "skills"}
              <p class={FIELD_LABEL}>Paste into your agent</p>
              {@render copyField("skills-prompt", SKILLS_PROMPT, "the prompt")}
              <p class={FIELD_LABEL}>Or pin them in your repo</p>
              {@render copyField(
                "skills-pin",
                SKILLS_PIN,
                "the install command",
                true,
              )}
              <a
                href="https://skills.internetcomputer.org/"
                target="_blank"
                rel="noopener noreferrer"
                class={["mt-1", TEXT_LINK]}
              >
                Browse the skills
                <ArrowUpRightIcon
                  class="size-3.5 rtl:-scale-x-100"
                  aria-hidden="true"
                />
              </a>
            {:else}
              <p class={FIELD_LABEL}>Add as a connector in Claude or ChatGPT</p>
              {@render copyField("mcp-url", MCP_URL, "the connector URL")}
              <p class={FIELD_LABEL}>Or in Claude Code</p>
              {@render copyField(
                "mcp-claude-code",
                MCP_CLAUDE_CODE,
                "the Claude Code command",
                true,
              )}
              <a
                href="https://internetcomputer.org/icp-mcp/"
                target="_blank"
                rel="noopener noreferrer"
                class={["mt-1", TEXT_LINK]}
              >
                You sign in with Internet Identity
                <ArrowUpRightIcon
                  class="size-3.5 rtl:-scale-x-100"
                  aria-hidden="true"
                />
              </a>
            {/if}
          </div>
        {/each}
      </div>
    </section>

    <!-- The icp CLI: four lines from install to a running app. -->
    <section class={["ii-shelf flex flex-col gap-4 @4xl:col-span-3", TILE]}>
      <span aria-hidden="true" class={ICON_CHIP}>
        <SquareTerminalIcon class="size-5" />
      </span>
      <div class="flex flex-col gap-1.5">
        <h3 class={TILE_TITLE}>Build on your computer</h3>
        <p class={TILE_BODY}>
          The icp command line makes a project, runs it on your machine and
          deploys it. Write it in Motoko or Rust, or in TypeScript or Python
          with the community's kits.
        </p>
      </div>
      {@render copyField("cli", CLI_COMMANDS, "the commands", true)}
      <a
        href="https://docs.internetcomputer.org/getting-started/quickstart/"
        target="_blank"
        rel="noopener noreferrer"
        class={["mt-auto pt-1", TEXT_LINK]}
      >
        Quickstart
        <ArrowUpRightIcon
          class="size-3.5 rtl:-scale-x-100"
          aria-hidden="true"
        />
      </a>
    </section>

    {#snippet ninjaIcon()}
      <span aria-hidden="true" class={ICON_CHIP}>
        <CodeXmlIcon class="size-5" />
      </span>
    {/snippet}
    {@render linkTile(
      "https://icp.ninja",
      "icp.ninja",
      "Try it in your browser",
      "ICP Ninja opens a sample in Motoko or Rust. Change it and deploy in a click, with nothing to install.",
      ninjaIcon,
      "@4xl:col-span-2",
    )}

    {#snippet junoIcon()}
      <!-- On the light plate app tiles use: Juno's black mark would vanish
           on a dark shelf. -->
      <span
        aria-hidden="true"
        class="grid size-11 shrink-0 place-items-center rounded-xl bg-white p-2 [color-scheme:light] shadow-[0_0_0_1px_rgb(0_0_0/0.06),0_1px_2px_rgb(0_0_0/0.06)] dark:shadow-[0_0_0_1px_rgb(255_255_255/0.08),0_1px_2px_rgb(0_0_0/0.4)]"
      >
        <img src={juno.logo} alt="" class="size-full object-contain" />
      </span>
    {/snippet}
    {@render linkTile(
      juno.url,
      "juno.build",
      "Firebase, but yours",
      "Juno gives you hosting, data, storage and functions in a container you own on the Internet Computer.",
      junoIcon,
      "@4xl:col-span-2",
    )}

    {#snippet passkeyIcon()}
      <span aria-hidden="true" class={ICON_CHIP}>
        <PasskeyIcon class="size-5" />
      </span>
    {/snippet}
    {@render linkTile(
      "https://docs.internetcomputer.org/guides/authentication/internet-identity/",
      "docs.internetcomputer.org",
      "Sign-in comes with it",
      "Add Internet Identity and people sign in with a passkey. Each app sees its own identity for them, so no one can follow them from app to app.",
      passkeyIcon,
      "@2xl:col-span-2 @4xl:col-span-2",
    )}

    <!-- Open Cloud: how far it goes, a whole cloud of your own. -->
    <a
      href="https://opencloud.org/#early-access"
      target="_blank"
      rel="noopener noreferrer"
      class={[
        "ii-shelf ii-hover group grid items-center gap-6 @2xl:col-span-2 @3xl:grid-cols-2 @4xl:col-span-6",
        TILE,
        FOCUS,
      ]}
    >
      <div class="flex flex-col gap-4">
        <div class="flex items-center gap-3">
          <AppLogo logo={openCloud.logo} size="card" />
          <span class="text-text-primary text-sm font-semibold">
            {openCloud.name}
          </span>
          <Badge size="sm">Early access</Badge>
        </div>
        <div class="flex flex-col gap-1.5">
          <p
            class="text-text-tertiary text-xs font-semibold tracking-[0.08em] uppercase"
          >
            How far it goes
          </p>
          <h3 class={["@2xl:text-2xl", TILE_TITLE]}>
            A cloud of your own that keeps running
          </h3>
          <p class={["max-w-[48ch]", TILE_BODY]}>
            Pick the machines, who runs them and in which countries. Your apps
            and data are on every one, so the engine carries on when some go
            down.
          </p>
        </div>
        <span
          aria-hidden="true"
          class="bg-fg-primary text-bg-primary w-fit rounded-full px-4 py-1.5 text-sm font-semibold"
        >
          Apply for early access
        </span>
      </div>

      <!-- The art: an engine across seven providers, one of them down. -->
      <div
        aria-hidden="true"
        class="rounded-2xl bg-(--ii-surface-3) p-4 shadow-(--ii-shadow-1)"
      >
        <div class="flex items-center justify-between gap-3">
          <span class="text-text-secondary truncate font-mono text-xs">
            cloud-engine-global
          </span>
          <span
            class="text-text-success-primary flex shrink-0 items-center gap-1.5 text-xs font-medium"
          >
            <span class="bg-fg-success-primary size-1.5 rounded-full"></span>
            Running
          </span>
        </div>
        <ul class="mt-4 grid grid-cols-7 gap-1.5">
          {#each NODES as node (node.code)}
            <li class="flex flex-col items-center gap-2">
              <span
                class={[
                  "flex h-14 w-full items-end justify-center rounded-lg pb-2",
                  node.down === true
                    ? "border-border-secondary border border-dashed"
                    : "bg-(--ii-selected)",
                ]}
              >
                <span
                  class={[
                    "size-1.5 rounded-full",
                    node.down === true
                      ? "bg-fg-quaternary"
                      : "bg-fg-success-primary",
                  ]}
                ></span>
              </span>
              <span class="text-text-tertiary font-mono text-[10px]">
                {node.code}
              </span>
            </li>
          {/each}
        </ul>
        <p class="text-text-tertiary mt-3 text-xs">
          6 of 7 machines up · every app still answering
        </p>
      </div>
    </a>

    <!-- Docs and guides: where to go once one of the ways above has you. -->
    <section
      class={[
        "ii-shelf flex flex-col gap-3 @2xl:col-span-2 @4xl:col-span-6",
        TILE,
      ]}
    >
      <div
        class="flex flex-wrap items-baseline justify-between gap-x-4 gap-y-1"
      >
        <h3 class={TILE_TITLE}>Docs and guides</h3>
        <a
          href="https://docs.internetcomputer.org/"
          target="_blank"
          rel="noopener noreferrer"
          class={TEXT_LINK}
        >
          All docs
          <ArrowUpRightIcon
            class="size-3.5 rtl:-scale-x-100"
            aria-hidden="true"
          />
        </a>
      </div>
      <ul class="-mx-3 grid @2xl:grid-cols-2 @4xl:grid-cols-4">
        {#each DOCS as doc (doc.url)}
          <li>
            <a
              href={doc.url}
              target="_blank"
              rel="noopener noreferrer"
              class={[
                "flex h-full flex-col gap-0.5 rounded-xl px-3 py-2.5 transition-colors duration-150 hover:bg-(--ii-hover) active:bg-(--ii-selected)",
                FOCUS,
              ]}
            >
              <span class="text-text-primary text-sm font-medium">
                {doc.title}
              </span>
              <span class="text-text-tertiary text-xs text-pretty">
                {doc.detail}
              </span>
            </a>
          </li>
        {/each}
      </ul>
      <p class="text-text-tertiary text-xs">
        Stuck? Ask on the
        <a
          href="https://forum.dfinity.org/"
          target="_blank"
          rel="noopener noreferrer"
          class={[
            "text-text-secondary hover:text-text-primary rounded-sm underline decoration-1 underline-offset-2",
            FOCUS,
          ]}>developer forum</a
        >.
      </p>
    </section>
  </div>

  <!-- Copying says so on the button; this says it to a screen reader. -->
  <span class="sr-only" aria-live="polite">
    {copied !== undefined ? "Copied" : ""}
  </span>
</div>

<style>
  /* The caret in Caffeine's prompt, as if it were still being typed. */
  .caret {
    display: inline-block;
    width: 2px;
    height: 1.1em;
    margin-inline-start: 2px;
    vertical-align: -0.2em;
    background: currentColor;
    animation: caret-blink 1.1s steps(1) infinite;
  }
  @keyframes caret-blink {
    50% {
      opacity: 0;
    }
  }
  @media (prefers-reduced-motion: reduce) {
    .caret {
      animation: none;
    }
  }
</style>
