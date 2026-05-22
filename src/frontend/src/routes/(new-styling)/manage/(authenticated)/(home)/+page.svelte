<script lang="ts">
  import type { PageProps } from "./$types";
  import type { Component } from "svelte";
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
  import {
    deriveSmartActions,
    type SmartActionId,
  } from "$lib/utils/smartActions";
  import { EMAIL_RECOVERY } from "$lib/state/featureFlags";

  const { data }: PageProps = $props();

  const name = $derived(
    data.identityInfo.name[0] ?? data.identityNumber.toString(),
  );

  const smartActions = $derived(
    deriveSmartActions(data.identityInfo, {
      emailRecoveryEnabled: $EMAIL_RECOVERY,
    }),
  );

  type SmartActionPresentation = {
    label: string;
    icon: Component;
    onclick: () => void;
  };

  // Each smart-action descriptor produced by `deriveSmartActions` is
  // mapped to its concrete button presentation here. Labels are
  // looked up through `$t` so they re-render on locale change; navigation
  // uses `goto` with a SvelteKit page state so target pages can
  // deep-link straight into the relevant wizard via `afterNavigate`.
  const presentations: Record<SmartActionId, SmartActionPresentation> =
    $derived({
      "add-access-method": {
        label: $t`Add access method`,
        icon: PlusIcon,
        onclick: () => goto("/manage/access", { state: { add: true } }),
      },
      "setup-email": {
        label: $t`Set up recovery email`,
        icon: MailIcon,
        onclick: () => goto("/manage/recovery", { state: { email: true } }),
      },
      "update-email": {
        label: $t`Update recovery email`,
        icon: MailIcon,
        onclick: () => goto("/manage/recovery", { state: { email: true } }),
      },
      "setup-phrase": {
        label: $t`Set up recovery phrase`,
        icon: ShieldIcon,
        onclick: () => goto("/manage/recovery", { state: { activate: true } }),
      },
      "reset-phrase": {
        label: $t`Reset recovery phrase`,
        icon: ShieldIcon,
        onclick: () => goto("/manage/recovery", { state: { reset: true } }),
      },
    });

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
  {#each smartActions as action (action.id)}
    {@const presentation = presentations[action.id]}
    {@const Icon = presentation.icon}
    <button
      onclick={presentation.onclick}
      class="btn btn-secondary btn-sm rounded-full"
    >
      <Icon class="size-3.5" />
      {presentation.label}
    </button>
  {/each}
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
