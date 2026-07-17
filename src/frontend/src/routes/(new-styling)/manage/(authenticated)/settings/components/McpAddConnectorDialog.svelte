<script lang="ts">
  import { onDestroy } from "svelte";
  import { TriangleAlertIcon, RotateCwIcon, CheckIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import HoldToConfirm from "$lib/components/ui/HoldToConfirm.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { parseMcpServerUrl, probeMcpServer } from "$lib/utils/mcpServer";

  interface Props {
    onClose: () => void;
    onSave: (url: string) => void;
    saving: boolean;
  }

  const { onClose, onSave, saving }: Props = $props();

  type VerifyState =
    | "idle"
    | "typing"
    | "checking"
    | "ok"
    | "unverified"
    | "invalid";

  let urlInput = $state("");
  let verifyState = $state<VerifyState>("idle");
  let parsedUrl = $state<string | undefined>(undefined);

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
    verifyState = "typing";
    parsedUrl = undefined;
    debounceTimer = setTimeout(() => {
      void verify(parsed.url);
    }, 500);
  };

  const canConfirm = $derived(
    !saving &&
      parsedUrl !== undefined &&
      (verifyState === "ok" || verifyState === "unverified"),
  );

  const handleConfirm = () => {
    if (parsedUrl === undefined) return;
    onSave(parsedUrl);
  };

  const errorText = $derived(
    verifyState === "invalid"
      ? $t`Enter a valid https URL (for example https://mcp.example.com/mcp).`
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
        {$t`Add AI access`}
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
    </ul>

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

    <HoldToConfirm
      label={$t`Hold to continue`}
      variant="primary"
      disabled={!canConfirm}
      onComplete={handleConfirm}
    />
  </div>
</Dialog>
