<script lang="ts">
  import Input from "$lib/components/ui/Input.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onSubmit: (address: string) => Promise<void>;
    initialError?: string;
    /** Case-insensitive pool of addresses the user has already verified
     *  on the share page. When the typed address matches one of these,
     *  render a non-blocking heads-up — recovery and verified are
     *  independent buckets, so setting the same address here means a
     *  second DKIM round-trip. Continue still works. */
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
        Identity. We'll ask you to send a signed email from this inbox to
        confirm.
      </Trans>
    </p>
  </header>
  <Input
    label={$t`Email address`}
    bind:value={address}
    placeholder="alice@example.com"
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
    <Alert
      variant="info"
      title={$t`This is already a verified email`}
      description={$t`Setting it as your recovery email adds it as a separate entry — the two are independent, so removing one won't affect the other.`}
    />
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
