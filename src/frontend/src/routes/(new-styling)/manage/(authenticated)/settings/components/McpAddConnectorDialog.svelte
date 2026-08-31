<script lang="ts">
  import { onDestroy } from "svelte";
  import {
    TriangleAlertIcon,
    RotateCwIcon,
    CheckIcon,
    GlobeIcon,
    MonitorIcon,
  } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import HoldToConfirm from "$lib/components/ui/HoldToConfirm.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import {
    LOCAL_MCP_SERVER_URL,
    parseMcpServerUrl,
    probeMcpServer,
  } from "$lib/utils/mcpServer";
  import { originOf } from "$lib/utils/mcpConfig";
  import { backendCanisterConfig } from "$lib/globals";

  interface Props {
    onClose: () => void;
    onSave: (url: string) => Promise<void>;
  }

  const { onClose, onSave }: Props = $props();

  type VerifyState =
    | "idle"
    | "typing"
    | "checking"
    | "ok"
    | "unverified"
    | "invalid"
    | "official";

  // Remote connectors are named by URL; a local one has nothing to enter. Its
  // address is fixed and its port isn't knowable in advance (a local server
  // binds a fresh one per sign-in), so it is a choice, not a text field — a URL
  // box here would have to accept `http://127.0.0.1` and reject
  // `http://127.0.0.1:8000`, and the reachability check beside it can't run
  // against loopback at all.
  type ConnectorKind = "remote" | "local";

  let kind = $state<ConnectorKind>("remote");
  let urlInput = $state("");
  let verifyState = $state<VerifyState>("idle");
  let parsedUrl = $state<string | undefined>(undefined);
  let saving = $state(false);

  const officialOrigin =
    backendCanisterConfig.mcp_official_url[0] !== undefined
      ? originOf(backendCanisterConfig.mcp_official_url[0])
      : undefined;

  let debounceTimer: ReturnType<typeof setTimeout> | undefined;
  let destroyed = false;

  onDestroy(() => {
    destroyed = true;
    clearTimeout(debounceTimer);
  });

  const normalizeUrl = (value: string): string =>
    value.trim().replace(/\/+$/, "");

  const verify = async (url: string) => {
    verifyState = "checking";
    parsedUrl = url;
    const ok = await probeMcpServer(url);
    if (!destroyed && normalizeUrl(urlInput) === url) {
      verifyState = ok ? "ok" : "unverified";
    }
  };

  const handleInput = () => {
    clearTimeout(debounceTimer);
    const trimmed = normalizeUrl(urlInput);
    if (trimmed === "") {
      verifyState = "idle";
      parsedUrl = undefined;
      return;
    }
    const parsed = parseMcpServerUrl(trimmed);
    if (parsed === undefined) {
      verifyState = "typing";
      parsedUrl = undefined;
      return;
    }
    // The official connector is already available without spending the one
    // custom slot on it, so there is nothing to add here.
    if (officialOrigin !== undefined && parsed.origin === officialOrigin) {
      verifyState = "official";
      parsedUrl = undefined;
      return;
    }
    verifyState = "typing";
    parsedUrl = undefined;
    debounceTimer = setTimeout(() => {
      void verify(parsed.url);
    }, 500);
  };

  const canConfirm = $derived(
    !saving &&
      (kind === "local" ||
        (parsedUrl !== undefined &&
          (verifyState === "ok" || verifyState === "unverified"))),
  );

  const handleConfirm = async () => {
    const url = kind === "local" ? LOCAL_MCP_SERVER_URL : parsedUrl;
    if (url === undefined) return;
    saving = true;
    try {
      await onSave(url);
    } finally {
      saving = false;
    }
  };

  const selectKind = (next: ConnectorKind) => {
    kind = next;
  };

  const errorText = $derived(
    verifyState === "invalid"
      ? $t`Enter a valid https URL (for example https://mcp.example.com/mcp).`
      : verifyState === "official"
        ? $t`That's the official connector. No need to customize.`
        : undefined,
  );

  const handleBlur = () => {
    const trimmed = normalizeUrl(urlInput);
    if (trimmed === "") return;
    if (parseMcpServerUrl(trimmed) === undefined) {
      verifyState = "invalid";
    }
  };
</script>

<Dialog {onClose} width="wider">
  <div class="flex flex-col gap-5">
    <FeaturedIcon size="lg" variant="info" class="self-start">
      <McpIcon class="size-6" />
    </FeaturedIcon>

    <div class="flex flex-col gap-2">
      <h2 class="text-text-primary text-xl font-medium">
        {backendCanisterConfig.mcp_official_url.length > 0
          ? $t`Customize AI access`
          : $t`Add AI access`}
      </h2>
      <p class="text-text-tertiary text-sm text-pretty">
        <Trans>
          To let AI ask questions and perform actions across your apps, add an
          <a
            href="https://modelcontextprotocol.io/docs/getting-started/intro"
            target="_blank"
            rel="noopener noreferrer"
            class="text-text-primary font-semibold hover:underline focus-visible:underline"
          >
            MCP connector
          </a> you trust.
        </Trans>
      </p>
    </div>

    <div
      class="flex flex-row gap-3"
      role="radiogroup"
      aria-label={$t`Connector type`}
    >
      {#each [{ value: "remote", icon: GlobeIcon, label: $t`Remote server`, hint: $t`Hosted, by URL` }, { value: "local", icon: MonitorIcon, label: $t`On this computer`, hint: $t`A local program` }] as option (option.value)}
        <button
          type="button"
          role="radio"
          aria-checked={kind === option.value}
          disabled={saving}
          onclick={() => selectKind(option.value as ConnectorKind)}
          class="flex flex-1 flex-col gap-1 rounded-lg border p-3 text-start {kind ===
          option.value
            ? 'border-border-brand bg-bg-brand-primary'
            : 'border-border-tertiary bg-bg-primary'}"
        >
          <option.icon class="text-fg-secondary size-4.5" aria-hidden="true" />
          <span class="text-text-primary text-sm font-semibold"
            >{option.label}</span
          >
          <span class="text-text-tertiary text-xs">{option.hint}</span>
        </button>
      {/each}
    </div>

    <ul class="my-2 flex flex-col gap-5">
      <li class="flex flex-row items-start gap-3">
        <TriangleAlertIcon
          class="text-fg-secondary mt-0.5 size-4.5 shrink-0"
          aria-hidden="true"
        />
        <div class="flex flex-col gap-0.5 text-sm">
          <span class="text-text-primary font-semibold">
            {$t`Only add trusted connectors`}
          </span>
          <span class="text-text-tertiary">
            {$t`Make sure the URL is from a source you trust.`}
          </span>
        </div>
      </li>
      <li class="flex flex-row items-start gap-3">
        <RotateCwIcon
          class="text-fg-secondary mt-0.5 size-4.5 shrink-0"
          aria-hidden="true"
        />
        <div class="flex flex-col gap-0.5 text-sm">
          <span class="text-text-primary font-semibold">
            {$t`Revoke access anytime`}
          </span>
          <span class="text-text-tertiary">
            {$t`Remove the connector to revoke AI access.`}
          </span>
        </div>
      </li>
      {#if kind === "local"}
        <li class="flex flex-row items-start gap-3">
          <MonitorIcon
            class="text-fg-secondary mt-0.5 size-4.5 shrink-0"
            aria-hidden="true"
          />
          <div class="flex flex-col gap-0.5 text-sm">
            <span class="text-text-primary font-semibold">
              {$t`Any local program can connect`}
            </span>
            <span class="text-text-tertiary">
              {$t`A local server picks a new port each time, so any program on this computer can ask to connect. Remove it when you're not using one.`}
            </span>
          </div>
        </li>
      {/if}
    </ul>

    {#if kind === "local"}
      <p class="text-text-tertiary text-sm text-pretty">
        {$t`Your local server tells you when it's ready to connect. Nothing to enter here.`}
      </p>
    {:else}
      <div class="flex flex-col gap-1.5">
        <span class="text-text-secondary text-sm font-medium">
          {$t`Connector URL`}
        </span>
        <div class="relative">
          <Input
            bind:value={urlInput}
            oninput={handleInput}
            onblur={handleBlur}
            placeholder="https://mcp.example.com/mcp"
            aria-label={$t`MCP server URL`}
            error={errorText}
            disabled={saving}
            autocomplete="off"
            autocapitalize="off"
            spellcheck={false}
            inputClass="pe-11 font-mono text-sm"
          />
          <div
            class="pointer-events-none absolute inset-y-0 inset-e-3 flex items-center"
          >
            {#if verifyState === "checking"}
              <ProgressRing class="text-fg-tertiary size-5" />
            {:else if verifyState === "ok"}
              <Tooltip label={$t`Reachable MCP server`}>
                <span
                  class="bg-bg-success-primary text-fg-success-primary pointer-events-auto flex size-5 items-center justify-center rounded-full"
                  aria-label={$t`Reachable MCP server`}
                >
                  <CheckIcon class="size-3.5" />
                </span>
              </Tooltip>
            {:else if verifyState === "unverified" && parsedUrl !== undefined}
              <Tooltip
                label={$t`Couldn't validate MCP support`}
                description={$t`We couldn't validate MCP support at this URL, but you can still trust it.`}
              >
                <span
                  class="border-border-secondary text-fg-tertiary pointer-events-auto flex size-5 items-center justify-center rounded-full border"
                  aria-label={$t`Couldn't validate MCP support`}
                >
                  <TriangleAlertIcon class="size-3" />
                </span>
              </Tooltip>
            {/if}
          </div>
        </div>
      </div>
    {/if}

    <HoldToConfirm
      label={$t`Hold to continue`}
      variant="primary"
      disabled={!canConfirm}
      onComplete={() => void handleConfirm()}
    />
  </div>
</Dialog>
