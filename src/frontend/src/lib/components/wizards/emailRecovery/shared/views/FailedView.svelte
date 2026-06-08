<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { MailXIcon, CopyIcon } from "@lucide/svelte";
  import { waitFor } from "$lib/utils/utils";
  import { SUPPORT_URL } from "$lib/config";

  interface Props {
    /** Human-readable summary of what went wrong. The wizard maps
     *  canister error variants to friendly strings before passing
     *  them in. */
    reason: string;
    /** Strictly-public, copyable diagnostics blob (includes the
     *  gateway `message_id`) the user can hand to support. Absent when
     *  there's nothing useful to share (e.g. a purely FE-side failure
     *  with no canister challenge to read). See `../diagnostics.ts`. */
    diagnostics?: string;
    onRetry: () => void;
  }

  const { reason, diagnostics, onRetry }: Props = $props();

  // One-shot "Copied" state. `navigator.clipboard.writeText` rejects in
  // some embedded/insecure contexts; swallow it rather than throwing an
  // unhandled rejection (mirrors SendConfirmationEmail's copy helpers).
  let copied = $state(false);
  const copyDiagnostics = async () => {
    if (diagnostics === undefined) {
      return;
    }
    try {
      await navigator.clipboard.writeText(diagnostics);
    } catch {
      return;
    }
    copied = true;
    await waitFor(700);
    copied = false;
  };
</script>

<div class="flex flex-col gap-6">
  <header class="flex flex-col items-center gap-3 text-center">
    <MailXIcon class="text-fg-error-primary size-10" />
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Verification failed`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">{reason}</p>
  </header>
  <button class="btn btn-primary btn-lg" type="button" onclick={onRetry}>
    {$t`Try again`}
  </button>
  {#if diagnostics !== undefined}
    <div class="flex flex-col items-center gap-3 text-center">
      <button
        class="btn btn-tertiary btn-sm"
        type="button"
        onclick={copyDiagnostics}
      >
        <CopyIcon class="size-4" />
        {copied ? $t`Copied` : $t`Copy error details`}
      </button>
      <p class="text-text-tertiary text-sm">
        {$t`If this keeps happening, copy these details and include them in a`}
        <a
          class="text-text-primary underline"
          href={SUPPORT_URL}
          target="_blank"
          rel="noopener noreferrer"
        >
          {$t`support request`}</a
        >.
      </p>
    </div>
  {/if}
</div>
