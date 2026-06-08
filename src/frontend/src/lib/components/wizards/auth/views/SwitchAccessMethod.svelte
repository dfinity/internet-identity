<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import SsoIcon from "$lib/components/icons/SsoIcon.svelte";
  import { ArrowRightIcon, RepeatIcon, UserIcon } from "@lucide/svelte";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { getMetadataString, findConfig } from "$lib/utils/openID";
  import type { MethodTag } from "$lib/flows/authFlow.svelte";

  export type AccessMethod =
    | { type: "passkey" }
    | { type: "openid"; logo: string; name: string }
    | { type: "sso"; name: string };

  interface Props {
    previousIdentityNumber: bigint;
    newMethod: MethodTag;
    providerIssuer?: string;
    providerName?: string;
    onSwitch: () => void;
    onCancel?: () => void;
  }

  let {
    previousIdentityNumber,
    newMethod,
    providerIssuer,
    providerName,
    onSwitch,
    onCancel,
  }: Props = $props();

  const previous: LastUsedIdentity | undefined = $derived(
    $lastUsedIdentitiesStore.identities[previousIdentityNumber.toString()],
  );

  const previousEmail: string | undefined = $derived.by(() => {
    if (previous === undefined) return undefined;
    if (
      "openid" in previous.authMethod &&
      previous.authMethod.openid.metadata !== undefined
    ) {
      return getMetadataString(previous.authMethod.openid.metadata, "email");
    }
    if ("sso" in previous.authMethod) {
      return previous.authMethod.sso.email;
    }
    return undefined;
  });

  const fromMethod: AccessMethod | undefined = $derived.by(() => {
    if (previous === undefined) return undefined;
    const m = previous.authMethod;
    if ("passkey" in m) return { type: "passkey" };
    if ("openid" in m) {
      // findConfig handles template issuers (e.g. Microsoft's
      // `.../{tenantid}/v2.0`) that bare equality misses, so the
      // provider logo renders even when the stored `iss` is concrete.
      const config = findConfig(
        m.openid.iss,
        undefined,
        m.openid.metadata ?? [],
      );
      return {
        type: "openid",
        logo: config?.logo ?? "",
        name: config?.name ?? m.openid.iss,
      };
    }
    return { type: "sso", name: m.sso.name ?? m.sso.domain };
  });

  const toMethod: AccessMethod = $derived.by(() => {
    if (newMethod === "passkey") return { type: "passkey" };
    if (newMethod === "openid") {
      const config =
        providerIssuer === undefined
          ? undefined
          : findConfig(providerIssuer, undefined, []);
      return {
        type: "openid",
        logo: config?.logo ?? "",
        name: config?.name ?? providerName ?? providerIssuer ?? "",
      };
    }
    return { type: "sso", name: providerName ?? "" };
  });

  const userName: string = $derived(
    previous?.name ??
      previousEmail ??
      (previous !== undefined ? `${previous.identityNumber}` : ""),
  );
  const userEmail: string | undefined = $derived(
    previous?.name !== undefined ? previousEmail : undefined,
  );

  const methodLabel = (m: AccessMethod): string =>
    m.type === "passkey" ? $t`Passkey` : m.name;
</script>

{#if fromMethod !== undefined}
  <div class="flex flex-col">
    <div class="flex flex-col items-start">
      <h2
        class="text-text-primary text-[22px] leading-[26.4px] font-medium tracking-tight"
      >
        {$t`Switch access method`}
      </h2>
      <p class="text-text-tertiary mt-2 max-w-80 text-sm leading-5">
        {$t`This account is already attached to your identity. Your active access method will be switched to it.`}
      </p>
    </div>

    <div class="mt-4 flex flex-col items-center py-5">
      <div class="flex items-center gap-3">
        {#each [fromMethod, toMethod] as method, i (i)}
          {#if i > 0}
            <ArrowRightIcon
              class="text-fg-tertiary size-5 rtl:-scale-x-100"
              aria-hidden="true"
            />
          {/if}
          <span class="relative inline-block size-14 shrink-0">
            <span
              class="bg-bg-primary border-border-secondary text-fg-disabled flex size-14 items-center justify-center rounded-full border"
            >
              <UserIcon class="size-6" aria-hidden="true" />
            </span>
            <span
              class="bg-bg-primary_alt border-border-secondary text-fg-tertiary absolute -inset-e-1 -bottom-1 flex size-7 items-center justify-center rounded-full border"
            >
              {#if method.type === "passkey"}
                <PasskeyIcon class="size-4!" aria-hidden="true" />
              {:else if method.type === "openid"}
                <span class="size-4">
                  <!-- eslint-disable-next-line svelte/no-at-html-tags -- method.logo is a trusted SVG string from openid_configs -->
                  {@html method.logo}
                </span>
              {:else}
                <SsoIcon class="size-4!" aria-hidden="true" />
              {/if}
            </span>
          </span>
        {/each}
      </div>
      <div class="mt-2 text-center">
        <div class="text-text-primary text-base leading-tight font-semibold">
          {userName}
        </div>
        {#if userEmail !== undefined}
          <div class="text-text-tertiary mt-0.5 text-[13px]">{userEmail}</div>
        {/if}
      </div>
      <span
        class="bg-bg-primary border-border-secondary text-text-tertiary mt-2 inline-flex items-center gap-1.5 rounded-full border px-2.5 py-1 text-xs font-semibold"
      >
        {methodLabel(fromMethod)}
        <ArrowRightIcon class="size-3.5 rtl:-scale-x-100" aria-hidden="true" />
        {methodLabel(toMethod)}
      </span>
    </div>

    <button onclick={onSwitch} class="btn btn-primary btn-lg mt-3 w-full gap-2">
      <RepeatIcon class="size-4" aria-hidden="true" />
      {$t`Switch method`}
    </button>

    {#if onCancel !== undefined}
      <button
        onclick={onCancel}
        class="text-text-tertiary hover:text-text-primary mt-3 self-center text-sm font-semibold outline-0 hover:underline focus-visible:underline"
      >
        {$t`Use a different method`}
      </button>
    {/if}
  </div>
{/if}
