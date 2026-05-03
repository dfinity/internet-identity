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
    CircleXIcon,
    CircleCheckIcon,
  } from "@lucide/svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import { formatRelative, plural, t } from "$lib/stores/locale.store";
  import {
    CosePublicKey,
    DiscoverablePasskeyIdentity,
    authDataToCoseDebug,
    creationOptions,
    getRpId,
  } from "$lib/utils/discoverablePasskeyIdentity";
  import borc from "borc";
  import { aaguidToString } from "$lib/utils/webAuthn";
  import Badge from "$lib/components/ui/Badge.svelte";
  import type { Provider } from "$lib/assets/aaguid";
  import { onMount } from "svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import {
    anonymousActor,
    anonymousAgent,
    frontendCanisterConfig,
  } from "$lib/globals";
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
  import { toHex, waitFor } from "$lib/utils/utils";
  import { handleError } from "$lib/components/utils/error";
  import { SvelteMap } from "svelte/reactivity";

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
    debug?: {
      credentialIdHex: string;
      rawCoseHex: string;
      cleanedCoseHex: string;
      derHex: string;
      allEntries: Array<{ key: number | string; valueCborHex: string }>;
      filteredEntries: Array<{ key: number | string; valueCborHex: string }>;
      icCall?: {
        senderPubkeyHex: string;
        delegationPubkeyHex: string;
        callerPrincipal?: string;
        error?: string;
      };
    };
  }

  let identityResults = $state<IdentityResult[]>([]);
  let knownProviders = $state<Record<string, Provider>>({});
  let testResults = $state<TestResult[]>([]);
  let highlightIdentity = $state<bigint>();
  const knownCredentials = new SvelteMap<
    string,
    { publicKey: CosePublicKey; aaguid?: string }
  >();

  const missingUpgradedPasskey = (devices: DeviceData[]) =>
    // Has at least one passkey (else likely OpenID sign-in identity)
    devices.some((device) => device.credential_id[0] !== undefined) &&
    // Has no passkeys with the upgraded origin
    !devices.some(
      (device) =>
        device.credential_id[0] !== undefined &&
        device.origin[0]?.endsWith("id.ai") === true,
    );
  const lookupByPasskey = async () => {
    let identityNumber = BigInt(-1);
    const passkeyIdentity = DiscoverablePasskeyIdentity.useExisting({
      getPublicKey: async (result) => {
        const lookupResult = (
          await anonymousActor.lookup_device_key(new Uint8Array(result.rawId))
        )[0];
        if (lookupResult === undefined) {
          toaster.error({
            title: $t`Identity not found`,
            description: $t`This passkey is no longer associated with any identity.`,
          });
          // Hang intentionally so further sign-in code doesn't run after the
          // toaster surfaces the not-found state.
          return new Promise<CosePublicKey>(() => {});
        }
        identityNumber = lookupResult.anchor_number;
        return CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey));
      },
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
    const identityResult: IdentityResult = {
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
    };
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
      let debug: ReturnType<typeof authDataToCoseDebug> | undefined;
      const rpId = getRpId();
      const identity = new DiscoverablePasskeyIdentity({
        credentialCreationOptions: creationOptions(
          `self-service (Test passkey – safe to delete)`,
          rpId,
        ),
        getPublicKey: (result) => {
          if (result.response.attestationObject === undefined) {
            throw new Error("Was expecting an attestation response.");
          }
          const attObject = borc.decodeFirst(
            new Uint8Array(result.response.attestationObject),
          );
          const authData = new Uint8Array(attObject.authData);
          debug = authDataToCoseDebug(authData);
          return Promise.resolve(new CosePublicKey(debug.cleanedCose));
        },
      });
      await identity.sign(Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)));
      if (debug === undefined) throw new Error("Debug info missing");
      const credentialId = identity.getCredentialId()!;
      const aaguid = identity.getAaguid();
      const cosePublicKey = identity.getPublicKey() as CosePublicKey;
      const derHex = toHex(new Uint8Array(cosePublicKey.toDer()));

      // Attempt an actual IC canister call using a delegation from this passkey.
      // This triggers full IC ingress validation of the COSE key — if the key is
      // malformed (e.g. contains key_ops) the IC rejects the call here.
      const passkeyIdentity = DiscoverablePasskeyIdentity.useExisting({
        credentialIds: [credentialId],
        getPublicKey: () => Promise.resolve(cosePublicKey),
      });
      const sessionIdentity = await ECDSAKeyIdentity.generate();
      let icCall:
        | {
            senderPubkeyHex: string;
            delegationPubkeyHex: string;
            callerPrincipal?: string;
            error?: string;
          }
        | undefined;
      try {
        const delegationChain = await DelegationChain.create(
          passkeyIdentity,
          sessionIdentity.getPublicKey(),
        );
        const senderPubkeyHex = toHex(
          new Uint8Array(delegationChain.publicKey),
        );
        const delegationPubkeyHex = toHex(
          new Uint8Array(
            delegationChain.delegations[0]?.delegation.pubkey ?? [],
          ),
        );
        const delegationIdentity = DelegationIdentity.fromDelegation(
          sessionIdentity,
          delegationChain,
        );
        const agent = await HttpAgent.from(anonymousAgent);
        agent.replaceIdentity(delegationIdentity);
        const principal = await anonymousActor.whoami.withOptions({ agent })();
        icCall = {
          senderPubkeyHex,
          delegationPubkeyHex,
          callerPrincipal: principal.toText(),
        };
      } catch (icError) {
        // Re-throw user cancellations — the test was not completed
        if (
          icError instanceof DOMException &&
          icError.name === "NotAllowedError"
        ) {
          throw icError;
        }
        icCall = {
          senderPubkeyHex: derHex,
          delegationPubkeyHex: toHex(
            new Uint8Array(sessionIdentity.getPublicKey().toDer()),
          ),
          error: icError instanceof Error ? icError.message : String(icError),
        };
      }

      const resolvedAaguid =
        aaguid !== undefined ? aaguidToString(aaguid) : undefined;
      knownCredentials.set(toHex(new Uint8Array(credentialId)), {
        publicKey: cosePublicKey,
        aaguid: resolvedAaguid,
      });
      testResults.push({
        aaguid: resolvedAaguid,
        date: Date.now(),
        debug: {
          credentialIdHex: toHex(credentialId),
          rawCoseHex: debug.rawCoseHex,
          cleanedCoseHex: debug.cleanedCoseHex,
          allEntries: debug.allEntries,
          filteredEntries: debug.filteredEntries,
          derHex,
          icCall,
        },
      });
    } catch (error) {
      handleError(error);
    }
  };
  const testDiscoverableAuth = async () => {
    try {
      let resolvedCredential:
        | { publicKey: CosePublicKey; aaguid?: string }
        | undefined;
      const discoverableIdentity = DiscoverablePasskeyIdentity.useExisting({
        getPublicKey: (result) => {
          const credHex = toHex(new Uint8Array(result.rawId));
          resolvedCredential = knownCredentials.get(credHex);
          if (resolvedCredential === undefined) {
            throw new Error(
              "Run 'Test passkey support' first to register this passkey.",
            );
          }
          return Promise.resolve(resolvedCredential.publicKey);
        },
      });
      await discoverableIdentity.sign(
        Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)),
      );
      if (resolvedCredential === undefined)
        throw new Error("Credential not resolved");
      const credentialId = discoverableIdentity.getCredentialId()!;
      const cosePublicKey = resolvedCredential.publicKey;
      const derHex = toHex(new Uint8Array(cosePublicKey.toDer()));

      const passkeyIdentity = DiscoverablePasskeyIdentity.useExisting({
        credentialIds: [credentialId],
        getPublicKey: () => Promise.resolve(cosePublicKey),
      });
      const sessionIdentity = await ECDSAKeyIdentity.generate();
      let icCall:
        | {
            senderPubkeyHex: string;
            delegationPubkeyHex: string;
            callerPrincipal?: string;
            error?: string;
          }
        | undefined;
      try {
        const delegationChain = await DelegationChain.create(
          passkeyIdentity,
          sessionIdentity.getPublicKey(),
        );
        const senderPubkeyHex = toHex(
          new Uint8Array(delegationChain.publicKey),
        );
        const delegationPubkeyHex = toHex(
          new Uint8Array(
            delegationChain.delegations[0]?.delegation.pubkey ?? [],
          ),
        );
        const delegationIdentity = DelegationIdentity.fromDelegation(
          sessionIdentity,
          delegationChain,
        );
        const agent = await HttpAgent.from(anonymousAgent);
        agent.replaceIdentity(delegationIdentity);
        const principal = await anonymousActor.whoami.withOptions({ agent })();
        icCall = {
          senderPubkeyHex,
          delegationPubkeyHex,
          callerPrincipal: principal.toText(),
        };
      } catch (icError) {
        if (
          icError instanceof DOMException &&
          icError.name === "NotAllowedError"
        ) {
          throw icError;
        }
        icCall = {
          senderPubkeyHex: derHex,
          delegationPubkeyHex: toHex(
            new Uint8Array(sessionIdentity.getPublicKey().toDer()),
          ),
          error: icError instanceof Error ? icError.message : String(icError),
        };
      }

      testResults.push({
        aaguid: resolvedCredential.aaguid,
        date: Date.now(),
        debug: {
          credentialIdHex: toHex(credentialId),
          rawCoseHex: "",
          cleanedCoseHex: toHex(cosePublicKey.getCose()),
          allEntries: [],
          filteredEntries: [],
          derHex,
          icCall,
        },
      });
    } catch (error) {
      handleError(error);
    }
  };
  const exportJSON = () => {
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
          ? toHex(value as Uint8Array)
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
    void import("$lib/assets/aaguid").then(
      (data) => (knownProviders = data.default),
    );
    // Load identity numbers from storage (old and new),
    // and fetch all devices for each anchor in parallel.
    void readStorage()
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
          {#each [...identityResults].sort((a, b) => b.lastUsed - a.lastUsed) as identityResult (identityResult.identityNumber)}
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
                  <span
                    class="text-text-primary overflow-hidden text-sm font-medium overflow-ellipsis whitespace-nowrap sm:text-base"
                  >
                    {identityResult.name}
                  </span>
                  <span class="text-text-tertiary me-4 text-sm sm:text-base">
                    {identityResult.identityNumber}
                  </span>
                {:else}
                  <span
                    class="text-text-primary me-4 text-sm font-medium sm:text-base"
                  >
                    {identityResult.identityNumber}
                  </span>
                {/if}
                {#if identityResult.legacy}
                  <Badge class="ms-auto whitespace-nowrap">
                    {$t`Not upgraded`}
                  </Badge>
                {/if}
              </div>
              <div class="flex flex-col gap-4 sm:flex-row">
                <div class="flex flex-col gap-2 sm:flex-row sm:items-center">
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
          <Trans>Check out the other domains:</Trans>
          <ul class="mt-1 flex flex-col gap-0.5">
            {#each (frontendCanisterConfig.related_origins[0] ?? []).filter((relatedOrigin) => relatedOrigin !== window.location.origin) as relatedOrigin (relatedOrigin)}
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
      <div class="flex flex-wrap gap-3">
        <button onclick={testPasskeyCreation} class="btn gap-2 max-sm:w-full">
          <PlayIcon class="size-5" />
          <span>{$t`Test passkey support`}</span>
        </button>
        <button
          onclick={testDiscoverableAuth}
          class="btn btn-secondary gap-2 max-sm:w-full"
        >
          <PlayIcon class="size-5" />
          <span>{$t`Test sign-in`}</span>
        </button>
      </div>
      <hr class="bt-1 border-border-secondary my-4" />
      {#if testResults.length > 0}
        <div class="flex flex-col gap-4">
          {#each [...testResults].reverse() as testResult (testResult.date)}
            {@const provider =
              knownProviders !== undefined && testResult.aaguid !== undefined
                ? knownProviders[testResult.aaguid]
                : undefined}
            <div
              class="border-border-secondary bg-bg-tertiary rounded-xl border p-4"
            >
              <div class="flex flex-row items-start">
                <div class="text-text-primary text-base font-medium">
                  {provider?.name ?? $t`Unknown`}
                </div>
                <Badge class="ms-auto !flex flex-row items-center gap-1">
                  {#if testResult.debug?.icCall?.error !== undefined}
                    <XIcon class="text-text-error-primary size-5" />
                    <span class="text-text-error-primary">{$t`Error`}</span>
                  {:else}
                    <CheckIcon class="text-text-success-primary size-5" />
                    <span class="text-text-success-primary"
                      >{$t`Supported`}</span
                    >
                  {/if}
                </Badge>
              </div>
              <div class="text-text-tertiary mb-4 text-sm">
                {testResult.aaguid ?? $t`Unknown`}
              </div>
              <div class="flex flex-col items-end gap-4 sm:flex-row">
                <div class="text-text-primary text-xs">
                  {#if testResult.debug?.icCall?.error !== undefined}
                    {$t`This passkey seems to have issues, please share the export with support.`}
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
