<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import { PencilIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import type { AuthnMethodData } from "$lib/generated/internet_identity_types";
  import { aaguidToString, getAuthnMethodAlias } from "$lib/utils/webAuthn";
  import { onMount, untrack } from "svelte";
  import type { Provider } from "$lib/assets/aaguid";

  interface Props {
    passkey: AuthnMethodData;
    onRename: (name: string) => Promise<void>;
    onCancel: () => void;
  }

  const { passkey, onRename, onCancel }: Props = $props();

  let knownProviders = $state<Record<string, Provider>>({});
  let isSubmitting = $state(false);
  let inputRef = $state<HTMLInputElement>();

  const initialName = getAuthnMethodAlias(passkey);
  const aaguid = $derived.by(() => {
    if (!("WebAuthn" in passkey.authn_method)) {
      return;
    }
    const [value] = passkey.authn_method.WebAuthn.aaguid;
    if (value === undefined) {
      return;
    }
    return aaguidToString(new Uint8Array(value));
  });
  const providerName = $derived(
    aaguid !== undefined ? knownProviders?.[aaguid]?.name : undefined,
  );

  let name = $state(untrack(() => initialName));

  const hasChanges = $derived(initialName !== name.trim());

  const handleSubmit = async () => {
    try {
      isSubmitting = true;
      await onRename(name.trim());
    } finally {
      isSubmitting = false;
    }
  };

  onMount(() => {
    inputRef?.focus();

    // Lazy load known providers data
    import("$lib/assets/aaguid").then(
      (data) => (knownProviders = data.default),
    );
  });
</script>

<form class="flex flex-1 flex-col">
  <fieldset class="flex flex-1 flex-col" disabled={isSubmitting}>
    <div class="mb-8 flex flex-col">
      <FeaturedIcon size="lg" class="mb-4 self-start">
        <PencilIcon class="size-6" />
      </FeaturedIcon>
      <h1 class="text-text-primary mb-3 text-2xl font-medium">
        {$t`Rename passkey`}
      </h1>
      <p class="text-text-tertiary mb-6 text-base font-medium">
        {$t`Give your passkey a memorable name to help you identify it.`}
      </p>
      <Input
        bind:element={inputRef}
        bind:value={name}
        inputmode="text"
        placeholder={providerName ?? $t`Passkey name`}
        type="text"
        autocomplete="off"
        autocorrect="off"
        spellcheck="false"
        error={name.length > 32
          ? $t`Maximum length is 32 characters.`
          : undefined}
        aria-label={$t`Passkey name`}
      />
    </div>
    <div class="mt-auto flex flex-col items-stretch gap-3">
      <Button
        type="submit"
        size="lg"
        onclick={handleSubmit}
        disabled={(providerName === undefined && name.trim().length === 0) ||
          name.length > 32 ||
          !hasChanges}
      >
        {#if isSubmitting}
          <ProgressRing />
          <span>{$t`Saving changes...`}</span>
        {:else}
          <span>{$t`Save changes`}</span>
        {/if}
      </Button>
      <Button onclick={onCancel} variant="tertiary" size="lg">
        {$t`Cancel`}
      </Button>
    </div>
  </fieldset>
</form>
