<script lang="ts">
  import type { PageProps } from "./$types";
  import type { Component } from "svelte";
  import { goto } from "$app/navigation";
  import {
    ArrowUpRightIcon,
    MailIcon,
    PlusIcon,
    ShieldAlertIcon,
    ShieldIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { frontendCanisterConfig } from "$lib/globals";
  import {
    getDapps,
    type KnownDapp,
  } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { deriveSmartActions, type SmartActionId } from "./smartActions";
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
      "verify-phrase": {
        label: $t`Verify recovery phrase`,
        icon: ShieldAlertIcon,
        onclick: () => goto("/manage/recovery", { state: { verify: true } }),
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
    // De-duplicate: `KnownDapp.hasOrigin` matches a dapp's main
    // website *or* any of its `authOrigins`, so a config entry
    // listing both the website and an alias would otherwise push
    // the same dapp twice and collide on the `{#each}` key below.
    // The configured list is small (single-digit), so a linear
    // `includes` check is fine and avoids reaching for a Map/Set.
    const matches: KnownDapp[] = [];
    for (const origin of origins) {
      const dapp = dapps.find((d) => d.hasOrigin(origin));
      if (dapp !== undefined && !matches.includes(dapp)) {
        matches.push(dapp);
      }
    }
    return matches;
  });
</script>

<!-- All three zones (welcome heading, smart-action strip, featured
     apps) share the same 640px column from the option-H design so
     the featured-app cards don't balloon to fill a wide manage
     pane on desktop. The wrapper is a grid (not flex) and the
     single column is declared `minmax(0, 1fr)`. That `0` floor
     forbids the column from growing to fit its content's intrinsic
     min-width — which is what was happening with the action strip
     pushing the page into horizontal scroll on narrow viewports.
     With this hard 0-min, the strip's own `overflow-x-auto` can
     finally take over. -->
<div class="grid w-full max-w-[40rem] grid-cols-[minmax(0,1fr)]">
  <header class="flex flex-col gap-3">
    <h1 class="text-text-tertiary text-3xl font-medium tracking-tight">
      <Trans>Welcome, <span class="text-text-primary">{name}</span>.</Trans>
    </h1>
  </header>

  <!-- Horizontal strip rather than wrap: on narrow viewports the
       three or four pills won't fit on one row, so we scroll them
       instead of stacking. The grid column above guarantees this
       block can't be wider than its parent, so `overflow-x-auto`
       reliably engages.
       On mobile the strip bleeds outside the page's own px-4
       padding via `-mx-4`, with `px-4` re-added on the inner row
       so the first/last buttons keep a 16px breathing margin and
       the partly-scrolled-off pills hit the viewport edge instead
       of getting clipped by the page padding. The custom selectors
       hide the scrollbar without disabling scrolling. -->
  <div
    class="-mx-4 mt-10 overflow-x-auto [scrollbar-width:none] sm:mx-0 [&::-webkit-scrollbar]:hidden"
  >
    <div class="flex gap-2 px-4 sm:px-0">
      {#each smartActions as action (action.id)}
        {@const presentation = presentations[action.id]}
        {@const Icon = presentation.icon}
        <button
          onclick={presentation.onclick}
          class="btn btn-secondary btn-sm shrink-0 rounded-full whitespace-nowrap"
        >
          <Icon class="size-3.5" />
          {presentation.label}
        </button>
      {/each}
    </div>
  </div>

  {#if featuredApps.length > 0}
    <!-- `@container` switches the grid breakpoint to a container
         query so the cards react to the *section's* width, not the
         viewport. That keeps them from squishing when the manage
         pane is narrowed by an open sidebar on a wide screen. -->
    <section class="@container mt-12 flex flex-col gap-3.5">
      <h2 class="text-text-primary text-base font-medium tracking-tight">
        {$t`Featured apps`}
      </h2>
      <div class="grid grid-cols-1 gap-3 @xl:grid-cols-3">
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
                alt={`${dapp.name} logo`}
                width="56"
                height="56"
                class="block size-14 rounded-xl"
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
</div>
