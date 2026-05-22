<script lang="ts">
  import type { PageProps } from "./$types";
  import { goto } from "$app/navigation";
  import {
    ArrowUpRightIcon,
    MailIcon,
    PlusIcon,
    ShieldIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { frontendCanisterConfig } from "$lib/globals";
  import {
    getDapps,
    type KnownDapp,
  } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { getMetadataString } from "$lib/utils/openID";

  const { data }: PageProps = $props();

  const name = $derived(
    data.identityInfo.name[0] ?? data.identityNumber.toString(),
  );

  const hasEmailRecovery = $derived(
    data.identityInfo.email_recovery[0] !== undefined,
  );

  const hasRecoveryPhrase = $derived(
    data.identityInfo.authn_methods.some(
      (m) =>
        "Recovery" in m.security_settings.purpose &&
        getMetadataString(m.metadata, "usage") === "recovery_phrase",
    ),
  );

  const featuredApps = $derived.by<KnownDapp[]>(() => {
    const origins = frontendCanisterConfig.featured_dashboard_apps[0] ?? [];
    if (origins.length === 0) return [];
    const dapps = getDapps();
    const matches: KnownDapp[] = [];
    for (const origin of origins) {
      const dapp = dapps.find((d) => d.hasOrigin(origin));
      if (dapp !== undefined) matches.push(dapp);
    }
    return matches;
  });
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-tertiary text-3xl font-medium tracking-tight">
    <Trans>Welcome back, <span class="text-text-primary">{name}</span>.</Trans>
  </h1>
</header>

<div class="mt-10 flex flex-wrap gap-2">
  <a href="/manage/access" class="btn btn-secondary btn-sm rounded-full">
    <PlusIcon class="size-3.5" />
    {$t`Add access method`}
  </a>
  {#if hasEmailRecovery}
    <a href="/manage/recovery" class="btn btn-secondary btn-sm rounded-full">
      <MailIcon class="size-3.5" />
      {$t`Update recovery email`}
    </a>
  {/if}
  {#if hasRecoveryPhrase}
    <button
      onclick={() => goto("/manage/recovery", { state: { reset: true } })}
      class="btn btn-secondary btn-sm rounded-full"
    >
      <ShieldIcon class="size-3.5" />
      {$t`Reset recovery phrase`}
    </button>
  {/if}
</div>

{#if featuredApps.length > 0}
  <section class="mt-12 flex flex-col gap-3.5">
    <h2 class="text-text-primary text-base font-medium tracking-tight">
      {$t`Featured apps`}
    </h2>
    <div class="grid grid-cols-1 gap-3 sm:grid-cols-2 lg:grid-cols-3">
      {#each featuredApps as dapp (dapp.website)}
        <a
          href={dapp.website}
          target="_blank"
          rel="noopener noreferrer"
          class="group bg-bg-primary_alt border-border-secondary hover:border-border-primary flex flex-col gap-5 rounded-xl border p-4 shadow-xs transition-colors"
        >
          <div class="flex items-start justify-between">
            <img
              src={dapp.logoSrc}
              alt=""
              width="48"
              height="48"
              class="block size-12 rounded-xl"
            />
            <ArrowUpRightIcon
              class="text-text-tertiary group-hover:text-text-primary size-4 transition-transform group-hover:translate-x-0.5 group-hover:-translate-y-0.5"
            />
          </div>
          <div class="flex flex-col gap-1">
            <span
              class="text-text-primary text-base font-semibold tracking-tight"
            >
              {dapp.name}
            </span>
            {#if dapp.oneLiner !== undefined}
              <span class="text-text-tertiary text-sm leading-snug">
                {dapp.oneLiner}
              </span>
            {/if}
          </div>
        </a>
      {/each}
    </div>
  </section>
{/if}
