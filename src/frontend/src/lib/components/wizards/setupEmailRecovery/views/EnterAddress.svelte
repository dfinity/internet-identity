<script lang="ts">
  import Input from "$lib/components/ui/Input.svelte";
  import Steps from "$lib/components/wizards/createRecoveryPhrase/components/Steps.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onSubmit: (address: string) => Promise<void>;
    initialError?: string;
  }

  const { onSubmit, initialError }: Props = $props();

  let address = $state("");
  let error = $state(initialError);
  let busy = $state(false);

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
  <div class="my-2"><Steps total={3} current={1} /></div>
  <header class="flex flex-col gap-2">
    <p class="text-text-tertiary text-xs font-medium tracking-wide uppercase">
      {$t`Add email recovery — step 1 of 3`}
    </p>
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
    autocomplete="email"
    spellcheck={false}
    {error}
    disabled={busy}
    autofocus
  />
  <button
    class="btn btn-primary btn-lg"
    type="submit"
    disabled={!isShapeValid || busy}
  >
    {busy ? $t`Verifying…` : $t`Continue`}
  </button>
</form>
