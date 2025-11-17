<script lang="ts">
  import type { PageProps } from "./$types";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import InactiveRecoveryPhrase from "./components/InactiveRecoveryPhrase.svelte";
  import { getMetadataString } from "$lib/utils/openID";
  import ActiveRecoveryPhrase from "./components/ActiveRecoveryPhrase.svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { CreateRecoveryPhraseWizard } from "$lib/components/wizards/createRecoveryPhrase";
  import { throwCanisterError } from "$lib/utils/utils";
  import { invalidateAll } from "$app/navigation";
  import UnverifiedRecoveryPhrase from "./components/UnverifiedRecoveryPhrase.svelte";
  import { recoveryAuthnMethodData } from "$lib/utils/authnMethodData";

  const { data }: PageProps = $props();

  let showRecoveryPhraseSetup = $state(false);
  let pendingRecoveryPhrase = $state<string[]>();
  let isUnverified = $state(false);

  let recoveryPhraseData = $derived(
    data.identityInfo.authn_methods.find(
      (m) =>
        "Recovery" in m.security_settings.purpose &&
        getMetadataString(m.metadata, "usage") === "recovery_phrase",
    ),
  );
  const isCurrentAccessMethod = $derived(
    "recoveryPhrase" in $authenticatedStore.authMethod,
  );

  const handleSetup = () => {
    pendingRecoveryPhrase = undefined;
    showRecoveryPhraseSetup = true;
  };
  const handleVerifyPending = () => {
    showRecoveryPhraseSetup = true;
  };
  const handleCreateOrReplace = async (recoveryPhrase: string[]) => {
    const data = await recoveryAuthnMethodData(recoveryPhrase);
    // Add new recovery phrase if there isn't one yet, else replace existing
    if (recoveryPhraseData === undefined) {
      await $authenticatedStore.actor
        .authn_method_add($authenticatedStore.identityNumber, data)
        .then(throwCanisterError);
    } else if ("PubKey" in recoveryPhraseData.authn_method) {
      await $authenticatedStore.actor
        .authn_method_replace(
          $authenticatedStore.identityNumber,
          recoveryPhraseData.authn_method.PubKey.pubkey,
          data,
        )
        .then(throwCanisterError);
    }
    pendingRecoveryPhrase = recoveryPhrase;
  };
  const handleCancel = () => {
    showRecoveryPhraseSetup = false;
  };
  const handleCompletePending = async () => {
    if (pendingRecoveryPhrase === undefined) {
      return;
    }
    showRecoveryPhraseSetup = false;
    recoveryPhraseData = await recoveryAuthnMethodData(pendingRecoveryPhrase);
    void invalidateAll();
  };
  const handleUnverified = async () => {
    await handleCompletePending();
    isUnverified = true;
  };
  const handleVerified = async () => {
    await handleCompletePending();
    isUnverified = false;
    pendingRecoveryPhrase = undefined;
  };
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Recovery phrase`}</h1>
  <p class="text-text-tertiary text-base">
    <Trans>A way to regain access to your identity if you ever lose it.</Trans>
  </p>
</header>

<!-- Uses same grid as cards below to match the width of all 3 cards -->
<div
  class="mt-10 grid grid-cols-[repeat(auto-fill,minmax(min(100%,20rem),1fr))] gap-5"
>
  <div class="col-span-3 max-sm:col-span-1">
    {#if isUnverified}
      <UnverifiedRecoveryPhrase
        onReset={handleSetup}
        onVerify={handleVerifyPending}
      />
    {:else if recoveryPhraseData !== undefined}
      <ActiveRecoveryPhrase
        onReset={handleSetup}
        recoveryPhrase={recoveryPhraseData}
        {isCurrentAccessMethod}
      />
    {:else}
      <InactiveRecoveryPhrase onActivate={handleSetup} />
    {/if}
  </div>
</div>

<section>
  <h2 class="text-text-primary mt-10 text-lg font-semibold">
    {$t`How to stay secure`}
  </h2>
  <div
    class={[
      // Layout
      "mt-5 grid grid-cols-[repeat(auto-fill,minmax(min(100%,20rem),1fr))] gap-5",
      // Articles
      "[&_article]:border-border-secondary [&_article]:bg-bg-primary [&_article]:rounded-2xl [&_article]:border [&_article]:p-5 [&_article]:not-dark:shadow-sm",
      // Headings
      "[&_h3]:text-text-primary [&_h3]:mb-2 [&_h3]:text-sm [&_h3]:font-semibold",
      // Paragraphs with emphasis
      "[&_p]:text-text-tertiary [&_p]:text-sm [&_p]:text-pretty",
      "[&_p_em]:text-text-primary [&_p_em]:normal [&_p_em]:font-medium [&_p_em]:not-italic",
    ]}
  >
    <article>
      <h3>{$t`Keep it secret`}</h3>
      <p>
        <Trans>
          Your recovery phrase gives <em>full control</em> over your identity.
          <em>Never</em> share it and <em>only</em> use it on the official website.
        </Trans>
      </p>
    </article>
    <article>
      <h3>{$t`Store it safely`}</h3>
      <p>
        <Trans>
          Write down all <em>24 words</em> correctly and keep them
          <em>offline</em>. Do <em>not</em> store them in the cloud or on devices.
        </Trans>
      </p>
    </article>
    <article>
      <h3>{$t`Replace it anytime`}</h3>
      <p>
        <Trans>
          You can <em>reset</em> your recovery phrase at any time. This keeps
          your identity <em>secure</em> if it is ever exposed.
        </Trans>
      </p>
    </article>
  </div>
</section>
{#if showRecoveryPhraseSetup}
  <Dialog onClose={pendingRecoveryPhrase ? handleUnverified : handleCancel}>
    <CreateRecoveryPhraseWizard
      onCreate={handleCreateOrReplace}
      onVerified={handleVerified}
      onCancel={handleCancel}
      unverifiedRecoveryPhrase={pendingRecoveryPhrase}
      hasExistingRecoveryPhrase={recoveryPhraseData !== undefined}
    />
  </Dialog>
{/if}
