<script lang="ts">
  import { TriangleAlertIcon, RotateCwIcon, CheckIcon } from "@lucide/svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import HoldToConfirm from "$lib/components/ui/HoldToConfirm.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import { t } from "$lib/stores/locale.store";
  import { parseMcpServerUrl, probeMcpServer } from "$lib/utils/mcpServer";

  interface Props {
    onClose: () => void;
    onSave: (url: string) => void;
    saving: boolean;
  }

  const { onClose, onSave, saving }: Props = $props();

  // Verify state:
  //  - "idle": nothing entered yet (or the user cleared it)
  //  - "typing": input has content but hasn't been parsed as https yet
  //  - "checking": a parsed https URL is being probed
  //  - "ok": the probe succeeded — server speaks MCP
  //  - "unverified": couldn't confirm it's an MCP server (advisory, still savable)
  //  - "invalid": the URL isn't a valid https URL
  type VerifyState =
    | "idle"
    | "typing"
    | "checking"
    | "ok"
    | "unverified"
    | "invalid";

  let urlInput = $state("");
  let verifyState = $state<VerifyState>("idle");
  // The parsed URL from the most recent successful parse — what we actually
  // save. Kept in sync with `urlInput` via the debounced verify.
  let parsedUrl = $state<string | undefined>(undefined);

  // Debounce the auto-verify so it doesn't fire on every keystroke. Cancelled
  // if the user edits again before it starts, and a stale probe result won't
  // land: `verify` guards on `parsed.url === urlInput` before assigning.
  let debounceTimer: ReturnType<typeof setTimeout> | undefined;

  const verify = async (url: string) => {
    verifyState = "checking";
    parsedUrl = url;
    const ok = await probeMcpServer(url);
    if (urlInput.trim() === url) {
      verifyState = ok ? "ok" : "unverified";
    }
  };

  const handleInput = () => {
    clearTimeout(debounceTimer);
    const trimmed = urlInput.trim();
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

  // Confirming is allowed as soon as the URL parses — the probe is advisory,
  // matching the pre-revamp behaviour (a server that can't be probed for CORS
  // reasons is still trustable). "checking" isn't confirmable because the
  // parsed URL isn't stored yet.
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
    const trimmed = urlInput.trim();
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
        {$t`To let AI ask questions and perform actions across your apps, add an MCP connector you trust.`}
      </p>
    </div>

    <ul class="flex flex-col gap-4">
      <li class="flex flex-row items-start gap-3">
        <TriangleAlertIcon
          class="text-fg-warning-primary mt-0.5 size-4.5 shrink-0"
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

    <div class="relative">
      <Input
        bind:value={urlInput}
        oninput={handleInput}
        onblur={handleBlur}
        label={$t`Connector URL`}
        placeholder="https://mcp.example.com/mcp"
        aria-label={$t`MCP server URL`}
        error={errorText}
        disabled={saving}
        autocomplete="off"
        autocapitalize="off"
        spellcheck={false}
        inputClass="pe-11 font-mono text-sm"
      />

      <span
        class="pointer-events-none absolute inset-e-3 top-8 flex size-6 items-center justify-center"
        aria-hidden={verifyState !== "ok" && verifyState !== "unverified"}
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
            label={$t`Couldn't verify this server`}
            description={$t`We couldn't confirm this URL speaks MCP, but you can still trust it.`}
          >
            <span
              class="border-border-secondary text-fg-tertiary pointer-events-auto flex size-5 items-center justify-center rounded-full border"
              aria-label={$t`Couldn't verify this server`}
            >
              <TriangleAlertIcon class="size-3" />
            </span>
          </Tooltip>
        {/if}
      </span>
    </div>

    <HoldToConfirm
      label={$t`Hold to continue`}
      onComplete={handleConfirm}
      class={canConfirm ? "" : "pointer-events-none opacity-60"}
    />
  </div>
</Dialog>
