<script lang="ts">
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onSubmit: (address: string) => Promise<void>;
    initialError?: string;
    verifiedAddresses?: string[];
  }

  const { onSubmit, initialError, verifiedAddresses = [] }: Props = $props();

  let address = $state("");
  let error = $state(initialError);
  let busy = $state(false);

  const normalized = $derived(address.trim().toLowerCase());

  // Lightweight client-side check — the canister does the
  // authoritative validation. We just want to flag the obvious
  // typos before issuing a network call.
  const isShapeValid = $derived(
    address.includes("@") &&
      !address.startsWith("@") &&
      !address.endsWith("@") &&
      !/\s/.test(address) &&
      address.length <= 254,
  );

  const overlapsVerified = $derived(
    isShapeValid &&
      verifiedAddresses.some((a) => a.toLowerCase() === normalized),
  );

  const handleSubmit = async (event: Event) => {
    event.preventDefault();
    if (!isShapeValid || busy) {
      return;
    }
    busy = true;
    error = undefined;
    try {
      await onSubmit(address.trim().toLowerCase());
    } catch (e) {
      error = e instanceof Error ? e.message : String(e);
    } finally {
      busy = false;
    }
  };
</script>

<form onsubmit={handleSubmit} class="flex flex-col gap-6">
  <header class="flex flex-col gap-2">
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Add a recovery email`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">
      <Trans>
        Type the email address you want to use to recover this Internet
        Identity. We'll ask you to send an email from this inbox to confirm.
      </Trans>
    </p>
  </header>
  <Input
    label={$t`Email address`}
    bind:value={address}
    placeholder="you@example.com"
    type="email"
    autocomplete="off"
    autocorrect="off"
    autocapitalize="off"
    spellcheck="false"
    data-1p-ignore
    data-lpignore="true"
    {error}
    disabled={busy}
    autofocus
  />
  {#if overlapsVerified}
    <div
      class="border-fg-warning-primary bg-bg-warning-primary flex flex-col gap-1 rounded-xl border p-4"
    >
      <div class="text-text-warning-primary text-sm font-semibold">
        {$t`Using a shareable email isn't recommended`}
      </div>
      <div class="text-text-warning-primary text-sm">
        <Trans>
          Keep your recovery email private and separate from emails you share
          with apps.
        </Trans>
      </div>
    </div>
  {/if}
  <button
    class="btn btn-primary btn-lg"
    type="submit"
    disabled={!isShapeValid || busy}
  >
    {#if busy}
      <ProgressRing />
      <span>{$t`Verifying…`}</span>
    {:else}
      <span>{$t`Continue`}</span>
    {/if}
  </button>
</form>
