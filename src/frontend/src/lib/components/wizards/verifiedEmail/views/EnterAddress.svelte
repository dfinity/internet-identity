<script lang="ts">
  import Input from "$lib/components/ui/Input.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onSubmit: (address: string) => Promise<void>;
    initialAddress?: string;
    /** When true, the input is locked to `initialAddress` — used by the
     *  Phase 1.5 "Verify from unverified" entry mode so a user can't
     *  silently swap the address being verified. */
    addressLocked?: boolean;
    initialError?: string;
    /** Case-insensitive pool of addresses bound to the user's recovery
     *  email slot. When the typed address matches one of these, render
     *  a non-blocking heads-up — recovery and verified are independent
     *  buckets, so adding the same address here means a second DKIM
     *  round-trip. Continue still works. */
    recoveryAddresses?: string[];
  }

  const {
    onSubmit,
    initialAddress,
    addressLocked = false,
    initialError,
    recoveryAddresses = [],
  }: Props = $props();

  let address = $state(initialAddress ?? "");
  let error = $state(initialError);
  let busy = $state(false);

  const normalized = $derived(address.trim().toLowerCase());

  const isShapeValid = $derived(
    address.includes("@") &&
      !address.startsWith("@") &&
      !address.endsWith("@") &&
      !/\s/.test(address) &&
      address.length <= 254,
  );

  const overlapsRecovery = $derived(
    isShapeValid &&
      recoveryAddresses.some((a) => a.toLowerCase() === normalized),
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
      {$t`Verify your email address`}
    </h1>
    <p class="text-text-tertiary text-base font-medium">
      <Trans>
        We'll ask you to send a signed email from this inbox to confirm.
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
    disabled={busy || addressLocked}
    autofocus={!addressLocked}
  />
  {#if overlapsRecovery}
    <Alert
      variant="warning"
      direction="horizontal"
      title={$t`This is your recovery email — best not to share it`}
      description={$t`Verifying it here lets apps request it. Your recovery email is meant to stay private so it can serve as a backup if you lose access to your identity.`}
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
