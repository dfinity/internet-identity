<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { MailIcon } from "@lucide/svelte";

  interface Props {
    onSubmit: (address: string) => Promise<void>;
    onCancel: () => void;
    initialError?: string;
  }

  const { onSubmit, onCancel, initialError }: Props = $props();

  let address = $state("");
  let error = $state(initialError);
  let busy = $state(false);

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

<form on:submit={handleSubmit} class="flex flex-col gap-6">
  <header class="flex flex-col items-center gap-3">
    <MailIcon class="text-fg-brand-primary size-10" />
    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Recover with email`}
    </h1>
    <p class="text-text-tertiary text-center text-sm">
      <Trans>
        Type the email address you registered as your recovery method. We'll
        ask you to send a signed email from that inbox to prove ownership.
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
    error={error}
    disabled={busy}
    autofocus
  />
  <div class="flex flex-row justify-end gap-2">
    <Button onclick={onCancel} variant="secondary" type="button" disabled={busy}>
      {$t`Cancel`}
    </Button>
    <Button type="submit" disabled={!isShapeValid || busy}>
      {busy ? $t`Verifying…` : $t`Continue`}
    </Button>
  </div>
</form>
