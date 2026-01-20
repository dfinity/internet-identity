<script lang="ts">
  import Trans from "$lib/components/locale/Trans.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import {
    SearchIcon,
    ShieldIcon,
    TestTubeDiagonalIcon,
    DownloadIcon,
    PlayIcon,
    CheckIcon,
    XIcon,
    TriangleAlertIcon,
    CircleXIcon,
    CircleCheckIcon,
  } from "@lucide/svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { formatRelative, plural, t } from "$lib/stores/locale.store";
  import {
    CosePublicKey,
    DiscoverablePasskeyIdentity,
  } from "$lib/utils/discoverablePasskeyIdentity";
  import Badge from "$lib/components/ui/Badge.svelte";
  import type { Provider } from "$lib/assets/aaguid";
  import { onMount } from "svelte";
  import { aaguidToString } from "$lib/utils/webAuthn";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { anonymousActor, anonymousAgent, canisterConfig } from "$lib/globals";
  import type { DeviceData } from "$lib/generated/internet_identity_types";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import { readStorage } from "$lib/legacy/storage";
  import { HttpAgent } from "@icp-sdk/core/agent";
  import {
    DelegationChain,
    DelegationIdentity,
    ECDSAKeyIdentity,
  } from "@icp-sdk/core/identity";
  import { toaster } from "$lib/components/utils/toaster";
  import { waitFor } from "$lib/utils/utils";
  import { bytesToHex } from "@noble/hashes/utils";
  import { handleError } from "$lib/components/utils/error";

  interface IdentityResult {
    identityNumber: bigint;
    legacy: boolean;
    devices: DeviceData[];
    lastUsed: number;
    name?: string;
  }

  interface TestResult {
    aaguid?: string;
    date: number;
  }

  const verifiedSupportedProviders = [
    "6028b017-b1d4-4c02-b4b3-afcdafc96bb2", // Windows Hello
    "9ddd1817-af5a-4672-a2b9-3e3dd95000a9", // Windows Hello
    "08987058-cadc-4b81-b6e1-30de50dcbe96", // Windows Hello
    "fbfc3007-154e-4ecc-8c0b-6e020557d7bd", // Apple Passwords
    "d548826e-79b4-db40-a3d8-11116f7e8349", // Bitwarden
    "bada5566-a7aa-401f-bd96-45619a55120d", // 1Password
    "ea9b8d66-4d01-1d21-3ce4-b6b48cb575d4", // Google Password Manager
    "53414d53-554e-4700-0000-000000000000", // Samsung Pass
    "a25342c0-3cdc-4414-8e46-f4807fca511c", // YubiKey latest firmware (5.7)
    "d7781e5d-e353-46aa-afe2-3ca49f13332a", // YubiKey latest firmware (5.7)
    "662ef48a-95e2-4aaa-a6c1-5b9c40375824", // YubiKey latest firmware (5.7)
    "19083c3d-8383-4b18-bc03-8f1c9ab2fd1b", // YubiKey latest firmware (5.7)
    "ff4dac45-ede8-4ec2-aced-cf66103f4335", // YubiKey latest firmware (5.7)
    "a02167b9-ae71-4ac7-9a07-06432ebb6f1c", // YubiKey latest firmware (5.7)
    "24673149-6c86-42e7-98d9-433fb5b73296", // YubiKey latest firmware (5.7)
    "fcc0118f-cd45-435b-8da1-9782b2da0715", // YubiKey latest firmware (5.7)
    "57f7de54-c807-4eab-b1c6-1c9be7984e92", // YubiKey latest firmware (5.7)
    "7b96457d-e3cd-432b-9ceb-c9fdd7ef7432", // YubiKey latest firmware (5.7)
    "dd86a2da-86a0-4cbe-b462-4bd31f57bc6f", // YubiKey latest firmware (5.7)
    "7409272d-1ff9-4e10-9fc9-ac0019c124fd", // YubiKey latest firmware (5.7)
    "90636e1f-ef82-43bf-bdcf-5255f139d12f", // YubiKey latest firmware (5.7)
    "34744913-4f57-4e6e-a527-e9ec3c4b94e6", // YubiKey latest firmware (5.7)
    "e77e3c64-05e3-428b-8824-0cbeb04b829d", // YubiKey latest firmware (5.7)
    "b7d3f68e-88a6-471e-9ecf-2df26d041ede", // YubiKey latest firmware (5.7)
    "47ab2fb4-66ac-4184-9ae1-86be814012d5", // YubiKey latest firmware (5.7)
    "ed042a3a-4b22-4455-bb69-a267b652ae7e", // YubiKey latest firmware (5.7)
  ];
  const verifiedUnsupportedProviders: Record<string, string> = {
    "cb69481e-8ff7-4039-93ec-0a2729a154a8": $t`This YubiKey has older firmware (5.1) that's incompatible.`,
    "fa2b99dc-9e39-4257-8f92-4a30d23c4118": $t`This YubiKey has older firmware (5.1) that's incompatible.`,
    "f8a011f3-8c0a-4d15-8006-17111f9edc7d": $t`This YubiKey has older firmware (5.1) that's incompatible.`,
    "531126d6-e717-415c-9320-3d9aa6981239": $t`Dashlane has been confirmed to be incompatible.`,
    "b84e4048-15dc-4dd0-8640-f4f60813c8af": $t`NordPass has been confirmed to be incompatible.`,
    "6e24d385-004a-16a0-7bfe-efd963845b34": $t`Ledger has been confirmed to be incompatible.`,
    "341e4da9-3c2e-8103-5a9f-aad887135200": $t`Ledger has been confirmed to be incompatible.`,
    "58b44d0b-0a7c-f33a-fd48-f7153c871352": $t`Ledger has been confirmed to be incompatible.`,
    "1d8cac46-47a1-3386-af50-e88ae46fe802": $t`Ledger has been confirmed to be incompatible.`,
    "fcb1bcb4-f370-078c-6993-bc24d0ae3fbe": $t`Ledger has been confirmed to be incompatible.`,
  };
  const possiblyUnsupportedProviders: Record<string, string> = {
    "b92c3f9a-c014-4056-887f-140a2501163b": $t`This YubiKey has older firmware (5.2) that might be incompatible.`,
    "ee882879-721c-4913-9775-3dfcce97072a": $t`This YubiKey has older firmware (5.2 - 5.4) that might be incompatible.`,
    "2fc0579f-8113-47ea-b116-bb5a8db9202a": $t`This YubiKey has older firmware (5.2 - 5.4) that might be incompatible.`,
    "c5ef55ff-ad9a-4b9f-b580-adebafe026d0": $t`This YubiKey has older firmware (5.2 - 5.4) that might be incompatible.`,
    "149a2021-8ef6-4133-96b8-81f8d5b7f1f5": $t`This YubiKey has older firmware (5.2 - 5.4) that might be incompatible.`,
    "c1f9a0bc-1dd2-404a-b27f-8e29047a43fd": $t`This YubiKey has older firmware (5.4) that might be incompatible.`,
    "73bb0cd4-e502-49b8-9c6f-b59445bf720b": $t`This YubiKey has older firmware (5.4) that might be incompatible.`,
    "85203421-48f9-4355-9bc8-8a53846e5083": $t`This YubiKey has older firmware (5.4) that might be incompatible.`,
    "a4e9fc6d-4cbe-4758-b8ba-37598bb5bbaa": $t`This YubiKey has older firmware (5.4) that might be incompatible.`,
    "0bb43545-fd2c-4185-87dd-feb0b2916ace": $t`This YubiKey has older firmware (5.4) that might be incompatible.`,
    "d8522d9f-575b-4866-88a9-ba99fa02f35b": $t`This YubiKey has older firmware (5.5 - 5.6) that might be incompatible.`,
    "7d1351a6-e097-4852-b8bf-c9ac5c9ce4a3": $t`This YubiKey has older firmware (5.6) that might be incompatible.`,
  };

  let identityResults = $state<IdentityResult[]>([]);
  let knownProviders = $state<Record<string, Provider>>({});
  let testResults = $state<TestResult[]>([]);
  let highlightIdentity = $state<bigint>();

  const missingUpgradedPasskey = (devices: DeviceData[]) =>
    // Has at least one passkey (else likely OpenID sign-in identity)
    devices.some((device) => device.credential_id[0] !== undefined) &&
    // Has no passkeys with the upgraded origin
    !devices.some(
      (device) =>
        device.credential_id[0] !== undefined &&
        device.origin[0]?.endsWith("id.ai"),
    );
  const lookupByPasskey = async () => {
    const identityResult = await new Promise<IdentityResult>(
      async (resolve) => {
        let identityNumber = BigInt(-1);
        const passkeyIdentity = await DiscoverablePasskeyIdentity.useExisting({
          getPublicKey: (result) =>
            new Promise(async (resolve) => {
              const lookupResult = (
                await anonymousActor.lookup_device_key(
                  new Uint8Array(result.rawId),
                )
              )[0];
              if (lookupResult === undefined) {
                toaster.error({
                  title: $t`Identity not found`,
                  description: $t`This passkey is no longer associated with any identity.`,
                });
                return;
              }
              identityNumber = lookupResult.anchor_number;
              resolve(
                CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey)),
              );
            }),
        });
        const sessionIdentity = await ECDSAKeyIdentity.generate();
        const delegationChain = await DelegationChain.create(
          passkeyIdentity,
          sessionIdentity.getPublicKey(),
        );
        const delegationIdentity = DelegationIdentity.fromDelegation(
          sessionIdentity,
          delegationChain,
        );
        const agent = await HttpAgent.from(anonymousAgent);
        agent.replaceIdentity(delegationIdentity);
        const info = await anonymousActor.get_anchor_info.withOptions({
          agent,
        })(identityNumber);
        resolve({
          identityNumber,
          legacy: missingUpgradedPasskey(info.devices),
          devices: info.devices,
          name: info.name[0],
          lastUsed: Math.max(
            ...info.devices
              .map((device) => device.last_usage[0])
              .filter((ns) => ns !== undefined)
              .map((ns) => Number(ns / BigInt(1_000_000))),
          ),
        });
      },
    );
    if (
      !identityResults.some(
        (result) => result.identityNumber === identityResult.identityNumber,
      )
    ) {
      identityResults.push(identityResult);
    }
    highlightIdentity = identityResult.identityNumber;
    await waitFor(2000);
    highlightIdentity = undefined;
  };
  const testPasskeyCreation = async () => {
    try {
      const identity = await DiscoverablePasskeyIdentity.createNew(
        `self-service (Test passkey – safe to delete)`,
      );
      const aaguid = await identity.getAaguid();
      testResults.push({
        aaguid: aaguid !== undefined ? aaguidToString(aaguid) : undefined,
        date: Date.now(),
      });
    } catch (error) {
      handleError(error);
    }
  };
  const exportJSON = async () => {
    const date = Date.now();
    const json = JSON.stringify(
      {
        origin: window.location.origin,
        date,
        identities: identityResults,
        tests: testResults,
      },
      (_, value) =>
        value instanceof Uint8Array
          ? bytesToHex(value as Uint8Array)
          : typeof value === "bigint"
            ? value.toString()
            : value,
      2,
    );
    const blob = new Blob([json], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `${window.location.hostname}-${date}-self-service.json`;
    a.click();
    URL.revokeObjectURL(url);
  };

  $effect(() => {
    if (highlightIdentity === undefined) {
      return;
    }
    document
      .querySelector(`[data-identity-result="${highlightIdentity}"]`)
      ?.scrollIntoView({ behavior: "smooth", block: "center" });
  });

  onMount(() => {
    // Lazy load known providers data
    import("$lib/assets/aaguid").then(
      (data) => (knownProviders = data.default),
    );
    // Load identity numbers from storage (old and new),
    // and fetch all devices for each anchor in parallel.
    readStorage()
      .then((storage) => [
        ...Object.entries(storage.anchors ?? {})
          // Filter out old entries that are in new storage
          .filter((entry) => {
            const [identityNumber] = entry;
            return !(identityNumber in $lastUsedIdentitiesStore.identities);
          })
          .map((entry) => {
            const [identityNumber, { lastUsedTimestamp }] = entry;
            return {
              identityNumber,
              name: undefined,
              lastUsed: lastUsedTimestamp,
            };
          }),
        // Add entries from new storage
        ...Object.entries($lastUsedIdentitiesStore.identities)
          .map((entry) => ({
            identityNumber: entry[0],
            name: entry[1].name,
            lastUsed: entry[1].lastUsedTimestampMillis,
          }))
          .sort((a, b) => b.lastUsed - a.lastUsed)
          // Limit new entries to latest 10 to avoid too many query calls
          .slice(0, 10),
      ])
      .then((identities) =>
        Promise.all(
          identities.map(async ({ identityNumber: key, name, lastUsed }) => {
            const identityNumber = BigInt(key);
            const devices = await anonymousActor.lookup(identityNumber);
            const legacy = missingUpgradedPasskey(devices);
            return { identityNumber, legacy, name, lastUsed, devices };
          }),
        ),
      )
      .then((newIdentityResults) => {
        identityResults.push(...newIdentityResults);
      });
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
      <button onclick={lookupByPasskey} class="btn gap-2 max-sm:w-full">
        <SearchIcon class="size-5" />
        <span>{$t`Lookup by passkey`}</span>
      </button>
      <hr class="bt-1 border-border-secondary my-4" />
      {#if identityResults.length > 0}
        <div class="mb-4 flex flex-col gap-4">
          {#each [...identityResults].sort((a, b) => b.lastUsed - a.lastUsed) as identityResult}
            {@const isRecoveryPhraseSetUp = identityResult.devices.some(
              (device) =>
                "recovery" in device.purpose &&
                "seed_phrase" in device.key_type,
            )}
            {@const passkeys = identityResult.devices.filter(
              (device) => device.credential_id[0] !== undefined,
            )}
            <div
              class={[
                "border-border-secondary bg-bg-tertiary rounded-xl border p-4 transition-all duration-200",
                highlightIdentity === identityResult.identityNumber &&
                  "!bg-bg-quaternary_hover",
              ]}
              data-identity-result={identityResult.identityNumber}
            >
              <div class="mb-1 flex flex-row items-center gap-2">
                {#if identityResult.name !== undefined}
                  <span class="text-text-primary text-base font-medium">
                    {identityResult.name}
                  </span>
                  <span class="text-text-tertiary text-base">
                    {identityResult.identityNumber}
                  </span>
                {:else}
                  <span class="text-text-primary text-base font-medium">
                    {identityResult.identityNumber}
                  </span>
                {/if}
                {#if identityResult.legacy}
                  <Badge size="lg" class="ms-auto">
                    {$t`Not upgraded`}
                  </Badge>
                {/if}
              </div>
              <div class="flex flex-row items-center gap-2">
                <div
                  class="text-text-tertiary flex flex-row items-center gap-1 text-sm"
                >
                  <PasskeyIcon class="!size-4" />
                  <span>
                    {$plural(passkeys.length, {
                      one: "# Passkey",
                      other: "# Passkeys",
                    })}
                  </span>
                </div>
                <div
                  class="text-text-tertiary flex flex-row items-center gap-1 text-sm"
                >
                  {#if isRecoveryPhraseSetUp}
                    <CircleCheckIcon
                      class="text-text-success-primary !size-4"
                    />
                    <span>
                      {$t`Recovery phrase set`}
                    </span>
                  {:else}
                    <CircleXIcon class="!size-4" />
                    <span>
                      {$t`No recovery phrase`}
                    </span>
                  {/if}
                </div>
                <div class="text-text-tertiary ms-auto text-sm">
                  {$formatRelative(new Date(identityResult.lastUsed), {
                    style: "long",
                  })}
                </div>
              </div>
            </div>
          {/each}
        </div>
      {/if}
      <div class="flex flex-col items-center justify-center py-4">
        <ShieldIcon class="text-fg-quaternary mb-3 size-16 stroke-1" />
        <div class="text-text-tertiary mb-2 text-center text-base font-medium">
          {$t`Can't find your identity?`}
        </div>
        <div class="text-text-tertiary text-sm text-balance">
          <Trans>Try the lookup and checkout the other domains:</Trans>
          <ul class="mt-1 flex flex-col gap-0.5">
            {#each (canisterConfig.related_origins[0] ?? []).filter((relatedOrigin) => relatedOrigin !== window.location.origin) as relatedOrigin}
              <li>
                <a
                  href={relatedOrigin + "/self-service"}
                  target="_blank"
                  class="text-text-primary hover:underline"
                >
                  {relatedOrigin}/self-service
                </a>
              </li>
            {/each}
          </ul>
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
          {#each [...testResults].reverse() as testResult}
            {@const provider =
              knownProviders !== undefined && testResult.aaguid !== undefined
                ? knownProviders[testResult.aaguid]
                : undefined}
            <div
              class="border-border-secondary bg-bg-tertiary rounded-xl border p-4"
            >
              <div class="flex flex-row items-center">
                <div class="text-text-primary text-base font-medium">
                  {provider?.name ?? $t`Unknown`}
                </div>
                <Badge
                  size="lg"
                  class="ms-auto !flex flex-row items-center gap-1"
                >
                  {#if testResult.aaguid === undefined}
                    <span>{$t`Unknown`}</span>
                  {:else if verifiedSupportedProviders.includes(testResult.aaguid)}
                    <CheckIcon class="text-text-success-primary size-5" />
                    <span class="text-text-success-primary">
                      {$t`Supported`}
                    </span>
                  {:else if testResult.aaguid in verifiedUnsupportedProviders}
                    <XIcon class="text-text-error-primary size-5" />
                    <span class="text-text-error-primary">
                      {$t`Unsupported`}
                    </span>
                  {:else if testResult.aaguid in possiblyUnsupportedProviders}
                    <TriangleAlertIcon
                      class="text-text-warning-primary size-5"
                    />
                    <span class="text-text-warning-primary">{$t`Warning`}</span>
                  {:else}
                    <span>{$t`Unknown`}</span>
                  {/if}
                </Badge>
              </div>
              <div class="text-text-tertiary mb-4 text-sm">
                {testResult.aaguid ?? $t`Unknown`}
              </div>
              <div class="flex flex-row items-end">
                <div class="text-text-primary text-xs">
                  {#if testResult.aaguid === undefined}
                    {$t`The passkey could not be identified, support unknown.`}
                  {:else if testResult.aaguid in verifiedUnsupportedProviders}
                    {verifiedUnsupportedProviders[testResult.aaguid]}
                  {:else if testResult.aaguid in possiblyUnsupportedProviders}
                    {possiblyUnsupportedProviders[testResult.aaguid]}
                  {:else if provider?.type === "cloud"}
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
                <div class="text-text-tertiary ms-auto text-sm">
                  {$formatRelative(new Date(testResult.date), {
                    style: "long",
                  })}
                </div>
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

  <button onclick={exportJSON} class="btn btn-secondary mx-auto my-8 gap-2">
    <DownloadIcon class="size-4" />
    <span>{$t`Export JSON`}</span>
  </button>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
