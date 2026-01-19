<script lang="ts">
  import Trans from "$lib/components/locale/Trans.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import {
    SearchIcon,
    ShieldIcon,
    TestTubeDiagonalIcon,
    DownloadIcon,
    PlayIcon,
  } from "@lucide/svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { t } from "$lib/stores/locale.store";
  import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
  import Badge from "$lib/components/ui/Badge.svelte";
  import type { Provider } from "$lib/assets/aaguid";
  import { onMount } from "svelte";
  import { aaguidToString } from "$lib/utils/webAuthn";

  interface TestResult {
    aaguid?: string;
  }

  let knownProviders = $state<Record<string, Provider>>({});
  let testResults = $state<TestResult[]>([]);

  const testPasskeyCreation = async () => {
    const identity =
      await DiscoverablePasskeyIdentity.createNew("Passkey testing");
    const aaguid = await identity.getAaguid();
    testResults.push({
      aaguid: aaguid !== undefined ? aaguidToString(aaguid) : undefined,
    });
  };

  onMount(() => {
    // Lazy load known providers data
    import("$lib/assets/aaguid").then(
      (data) => (knownProviders = data.default),
    );
  });
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center gap-4 px-4">
    <div
      class="bg-bg-secondary border-border-secondary w-full max-w-200 rounded-xl border p-6"
    >
      <h1
        class="text-text-primary mb-1 flex flex-row items-center gap-2 text-xl font-medium"
      >
        <ShieldIcon class="size-5" />
        {$t`Identity list`}
      </h1>
      <p class="text-text-tertiary mb-6 text-base text-pretty">
        <Trans>View your last used identities</Trans>
      </p>
      <button class="btn gap-2 max-sm:w-full">
        <SearchIcon class="size-5" />
        <span>{$t`Lookup by passkey`}</span>
      </button>
      <hr class="bt-1 border-border-secondary my-4" />
      <div class="flex flex-col items-center justify-center py-4">
        <ShieldIcon class="text-fg-quaternary mb-3 size-16 stroke-1" />
        <div class="text-text-tertiary mb-1 text-center text-base font-medium">
          {$t`No identities yet`}
        </div>
        <div class="text-text-tertiary text-center text-sm text-balance">
          {$t`Use the lookup button to add identities`}
        </div>
      </div>
    </div>
    <div
      class="bg-bg-secondary border-border-secondary w-full max-w-200 rounded-xl border p-6"
    >
      <h1
        class="text-text-primary mb-1 flex flex-row items-center gap-2 text-xl font-medium"
      >
        <TestTubeDiagonalIcon class="size-5" />
        {$t`Passkey testing`}
      </h1>
      <p class="text-text-tertiary mb-6 text-base text-pretty">
        <Trans>Test passkey support and view authenticator details</Trans>
      </p>
      <button onclick={testPasskeyCreation} class="btn gap-2 max-sm:w-full">
        <PlayIcon class="size-5" />
        <span>{$t`Test passkey support`}</span>
      </button>
      <hr class="bt-1 border-border-secondary my-4" />
      {#if testResults.length > 0}
        <div class="flex flex-col gap-4">
          {#each testResults as testResult}
            {@const provider =
              knownProviders !== undefined && testResult.aaguid !== undefined
                ? knownProviders[testResult.aaguid]
                : undefined}
            <div
              class="border-border-secondary bg-bg-tertiary rounded-xl border p-4"
            >
              <div class="flex flex-row items-center">
                <div class="text-text-primary text-base font-medium">
                  {provider.name}
                </div>
                <Badge class="ms-auto">Supported</Badge>
              </div>
              <div class="text-text-tertiary mb-4 text-base">
                {testResult.aaguid}
              </div>
              <div class="text-text-primary text-xs">
                {#if provider?.type === "cloud"}
                  {provider.platform === undefined
                    ? $t`Stored in your ${provider.account} account and synced across your devices.`
                    : $t`Stored in your ${provider.account} account and synced across your ${provider.platform} devices.`}
                {:else if provider?.type === "os"}
                  {$t`Stored and usable only on the ${provider.platform} device it was created on.`}
                {:else if provider?.type === "device"}
                  {$t`Kept on a physical key. Authenticate on supported devices via tap/insert.`}
                {:else if provider?.type === "browser"}
                  {provider.platform === undefined
                    ? $t`Stored and usable only in ${provider.browser} on the device it was created on.`
                    : $t`Stored and usable only in ${provider.browser} on the ${provider.platform} device it was created on.`}
                {:else}
                  {$t`Stored securely on your device, in your password manager, or on a security key.`}
                {/if}
              </div>
            </div>
          {/each}
        </div>
      {:else}
        <div class="flex flex-col items-center justify-center py-4">
          <TestTubeDiagonalIcon
            class="text-fg-quaternary mb-3 size-16 stroke-1"
          />
          <div
            class="text-text-tertiary mb-1 text-center text-base font-medium"
          >
            {$t`No passkey tests yet`}
          </div>
          <div class="text-text-tertiary text-center text-sm text-balance">
            {$t`Run a passkey test to see results`}
          </div>
        </div>
      {/if}
    </div>
  </div>

  <button class="btn btn-secondary mx-auto my-8 gap-2">
    <DownloadIcon class="size-4" />
    <span>{$t`Export JSON`}</span>
  </button>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
