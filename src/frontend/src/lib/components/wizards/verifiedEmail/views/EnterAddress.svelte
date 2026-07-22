<script lang="ts">
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onSubmit: (address: string) => Promise<void>;
    initialAddress?: string;
    addressLocked?: boolean;
    initialError?: string;
    recoveryAddresses?: string[];
    verifiedAddresses?: string[];
    openidAddresses?: string[];
  }

  const {
    onSubmit,
    initialAddress,
    addressLocked = false,
    initialError,
    recoveryAddresses = [],
    verifiedAddresses = [],
    openidAddresses = [],
  }: Props = $props();

  let address = $state(initialAddress ?? "");
  let submitError = $state(initialError);
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

  const isDuplicate = $derived(
    isShapeValid &&
      verifiedAddresses.some((a) => a.toLowerCase() === normalized),
  );

  const isOpenidDuplicate = $derived(
    isShapeValid &&
      !isDuplicate &&
      openidAddresses.some((a) => a.toLowerCase() === normalized),
  );

  const error = $derived(
    isDuplicate
      ? $t`You've already verified this email.`
      : isOpenidDuplicate
        ? $t`This email is already linked through an OpenID provider.`
        : submitError,
  );

  const handleSubmit = async (event: Event) => {
    event.preventDefault();
    if (!isShapeValid || isDuplicate || isOpenidDuplicate || busy) {
      return;
    }
    busy = true;
    submitError = undefined;
    try {
      await onSubmit(address.trim().toLowerCase());
    } catch (e) {
      submitError = e instanceof Error ? e.message : String(e);
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
      <Trans>We'll ask you to send an email from this inbox to confirm.</Trans>
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
    <div
      class="border-fg-warning-primary bg-bg-warning-primary flex flex-col gap-1 rounded-xl border p-4"
    >
      <div class="text-text-warning-primary text-sm font-semibold">
        {$t`This is already your recovery email.`}
      </div>
      <div class="text-text-warning-primary text-sm">
        <Trans>
          Please consider adding a different email that you may want to share
          with apps.
        </Trans>
      </div>
    </div>
  {/if}
  <button
    class="btn btn-primary btn-lg"
    type="submit"
    disabled={!isShapeValid || isDuplicate || isOpenidDuplicate || busy}
  >
    {#if busy}
      <ProgressRing />
      <span>{$t`Verifying…`}</span>
    {:else}
      <span>{$t`Continue`}</span>
    {/if}
  </button>
</form>
