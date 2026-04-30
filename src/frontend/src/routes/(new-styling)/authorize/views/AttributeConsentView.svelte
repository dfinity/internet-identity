<script lang="ts">
  import type {
    AttributeConsent,
    AttributeConsentContext,
  } from "$lib/stores/attributeConsent.store";
  import { SvelteMap } from "svelte/reactivity";
  import { extractScope } from "$lib/stores/channelHandlers/attributes";
  import { backendCanisterConfig } from "$lib/globals";
  import { discoverSsoConfig } from "$lib/utils/ssoDiscovery";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
  import AttributePicker from "./AttributePicker.svelte";
  import {
    type MergedGroup,
    groupId,
    mergeGroups,
  } from "./attributeConsentView.utils";

  interface Props {
    context: Promise<AttributeConsentContext>;
    variant: "openid" | "normal";
    onConsent: (consent: AttributeConsent) => void;
  }

  const { context, variant, onConsent }: Props = $props();

  /**
   * Cache of the published name for each `sso:<domain>` we've seen in
   * the consent groups, populated by re-running the same frontend
   * two-hop discovery the SSO sign-in path uses. The consent screen may
   * render `sso:<domain>:<key>` rows even when the user authenticated
   * through some other method (passkey, direct OpenID), so we can't
   * rely on a name being threaded through from sign-in.
   *
   * Populated up-front during the skeleton so the first row paint has
   * the discovered names — see the `prepared` promise below.
   */
  const ssoNamesByDomain = new SvelteMap<string, string>();

  const discoverSsoName = async (domain: string): Promise<void> => {
    if (ssoNamesByDomain.has(domain)) return;
    try {
      const result = await discoverSsoConfig(domain);
      if (result.name !== undefined && result.name.length > 0) {
        ssoNamesByDomain.set(domain, result.name);
      }
    } catch (error) {
      // Non-fatal: the SSO label falls back to the bare domain.
      console.error(`Failed to discover SSO name for ${domain}`, error);
    }
  };

  const ssoDomainsIn = (groups: MergedGroup[]): string[] => {
    // eslint-disable-next-line svelte/prefer-svelte-reactivity
    const domains = new Set<string>();
    for (const group of groups) {
      for (const opt of group.options) {
        const scope = extractScope(opt.display.key);
        if (scope !== undefined && scope.startsWith("sso:")) {
          domains.add(scope.slice("sso:".length));
        }
      }
    }
    return [...domains];
  };

  const getProviderName = (key: string): string | undefined => {
    const scope = extractScope(key);
    if (scope === undefined) return undefined;
    if (scope.startsWith("openid:")) {
      const issuer = scope.slice("openid:".length);
      return backendCanisterConfig.openid_configs[0]?.find(
        (c) => c.issuer === issuer,
      )?.name;
    }
    if (scope.startsWith("sso:")) {
      return ssoNamesByDomain.get(scope.slice("sso:".length));
    }
    return undefined;
  };

  const selections = new SvelteMap<
    string,
    { checked: boolean; selectedIndex: number }
  >();

  /** Widest label among rows whose `[checkbox][label][value][chevron]`
   *  natural single-line width fits in the picker. Threaded down to
   *  each `AttributePicker` so its wrap probe pads the label slot to
   *  this width — matching the col 2 width the actual grid will give
   *  rows once wrapped rows' labels span out of col 2. Computed only
   *  from rows that actually fit, so a long label whose value won't fit
   *  doesn't bloat the alignment column for everyone else. */
  let maxLabelWidth = $state(0);
  let labelProbeEl: HTMLDivElement | undefined = $state();

  $effect(() => {
    const probe = labelProbeEl;
    if (probe === undefined) return;
    const recompute = () => {
      let max = 0;
      for (const row of Array.from(probe.children)) {
        if (!(row instanceof HTMLElement)) continue;
        // Row "fits" iff its natural single-line width is ≤ the panel's
        // visible width — i.e. nothing overflows when rendered nowrap.
        if (row.scrollWidth > row.clientWidth) continue;
        const labelEl = row.querySelector("[data-label]");
        if (labelEl instanceof HTMLElement) {
          max = Math.max(max, labelEl.offsetWidth);
        }
      }
      maxLabelWidth = max;
    };
    recompute();
    const ro = new ResizeObserver(recompute);
    ro.observe(probe);
    return () => ro.disconnect();
  });

  /** Resolves only when the consent rows are fully ready to render:
   *  attribute groups computed, SSO provider names discovered, and
   *  `selections` initialised. Gating the template on this promise
   *  (rather than `context`) means `{:then}` never sees an in-between
   *  frame where a fast click could submit an empty consent result. */
  const prepared = (async () => {
    const ctx = await context;
    const groups = mergeGroups(ctx.groups);
    await Promise.all(ssoDomainsIn(groups).map(discoverSsoName));
    selections.clear();
    for (const group of groups) {
      selections.set(groupId(group), { checked: true, selectedIndex: 0 });
    }
    return { groups, effectiveOrigin: ctx.effectiveOrigin };
  })();

  const handleDenyAll = (groups: MergedGroup[]) => {
    for (const group of groups) {
      selections.set(groupId(group), { checked: false, selectedIndex: 0 });
    }
  };

  const handleContinue = (groups: MergedGroup[]) => {
    const attributes = groups.flatMap((group) => {
      const selection = selections.get(groupId(group));
      if (selection === undefined || !selection.checked) return [];
      return group.options[selection.selectedIndex].originals;
    });
    onConsent({ attributes });
  };

  /** Wrap a runtime value in Unicode bidi isolation marks so a Latin-script
   *  provider name embedded in an RTL label doesn't flip surrounding
   *  punctuation or colons. FSI + PDI keeps this invisible in `.po` files.
   *  Built at runtime so the source file itself contains no bidi control
   *  characters (which would trip review tooling). */
  const FSI = String.fromCharCode(0x2068);
  const PDI = String.fromCharCode(0x2069);
  const bidiIsolate = (value: string): string => `${FSI}${value}${PDI}`;

  /** Always emit a non-empty label for scoped rows. Prefer the published
   *  provider name (canister-configured for OpenID, two-hop discovery for
   *  SSO); fall back to the bare issuer URL or domain when neither is
   *  available so an explicitly scoped request never collapses into the
   *  unscoped fan-out's bare "Name:" / "Email:" label. */
  const scopedProviderLabel = (key: string): string | undefined => {
    const named = getProviderName(key);
    if (named !== undefined) return named;
    const scope = extractScope(key);
    if (scope === undefined) return undefined;
    if (scope.startsWith("openid:")) return scope.slice("openid:".length);
    if (scope.startsWith("sso:")) return scope.slice("sso:".length);
    return scope;
  };

  const labelForGroup = (group: MergedGroup): string => {
    const scopedProvider = !group.omitScope
      ? scopedProviderLabel(group.options[0].display.key)
      : undefined;
    // Bind interpolations to named consts so Lingui extracts the id as the
    // placeholder name (rather than `{0}`), giving translators clear context.
    const attribute = bidiIsolate(group.name);
    if (scopedProvider !== undefined) {
      const providerName = bidiIsolate(scopedProvider);
      switch (group.name) {
        case "email":
        case "verified_email":
          return $t`${providerName} email:`;
        case "name":
          return $t`${providerName} name:`;
        default:
          // The attribute name itself isn't in the translation catalog, but
          // the format (word order, colon convention) still is.
          return $t`${providerName} ${attribute}:`;
      }
    }
    switch (group.name) {
      case "email":
      case "verified_email":
        return $t`Email:`;
      case "name":
        return $t`Name:`;
      default:
        return $t`${attribute}:`;
    }
  };
</script>

{#await prepared}
  <div
    class="flex min-w-0 flex-1 flex-col"
    aria-busy="true"
    aria-label={$t`Loading permissions`}
  >
    <div class="flex flex-col items-center gap-6 py-5">
      <div class="skeleton size-20 rounded-2xl"></div>
      <div class="skeleton h-6 w-40 rounded-full"></div>
    </div>
    <div class="skeleton mb-2 h-8 w-52"></div>
    <div class="skeleton mb-6 h-5 w-60"></div>
    <div class="mb-4 flex flex-col gap-1">
      <div class="skeleton h-12 w-full rounded-lg"></div>
    </div>
    <div class="skeleton mb-6 h-4 w-16"></div>
    <div class="skeleton h-12 w-full rounded-lg"></div>
  </div>
{:then data}
  <div class="flex min-w-0 flex-1 flex-col">
    <AuthorizeHeader origin={data.effectiveOrigin} />
    <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
      {#if variant === "openid"}
        {$t`Review Permissions`}
      {:else}
        {$t`Allow to access this info`}
      {/if}
    </h1>
    <p class="text-text-secondary mb-6 self-start text-sm">
      {$t`Choose which details you'd like to share`}
    </p>

    <!-- Hidden probe panel: one full row per group, mirroring picker
         chrome (checkbox + label + value + optional chevron) on a
         single nowrap line at the panel's full width. The `$effect`
         above filters to rows whose natural width fits and takes the
         max of *those* labels — long labels whose values won't fit
         (so the row will wrap anyway) don't drive the alignment
         column. -->
    <div
      bind:this={labelProbeEl}
      aria-hidden="true"
      class="pointer-events-none invisible absolute inset-x-0 top-0 h-0 overflow-hidden"
    >
      {#each data.groups as group (groupId(group))}
        <div
          class="flex items-center gap-x-3 overflow-hidden px-3 whitespace-nowrap"
        >
          <span class="size-4 shrink-0"></span>
          <span class="shrink-0 text-sm" data-label>{labelForGroup(group)}</span
          >
          <span class="shrink-0 text-sm font-medium">
            {group.options[0].display.displayValue}
          </span>
          {#if group.options.length > 1}
            <span class="ms-auto size-6 shrink-0"></span>
          {/if}
        </div>
      {/each}
    </div>

    <div class="mb-4 grid w-full grid-cols-[auto_auto_1fr_auto] gap-y-1">
      {#each data.groups as group (groupId(group))}
        {@const id = groupId(group)}
        {@const selection = selections.get(id)}
        {#if selection !== undefined}
          <AttributePicker
            label={labelForGroup(group)}
            {maxLabelWidth}
            options={group.options.map((o) => ({
              id: o.display.key,
              value: o.display.displayValue,
              providerLabel: scopedProviderLabel(o.display.key),
            }))}
            selectedIndex={selection.selectedIndex}
            checked={selection.checked}
            onCheck={(checked) => {
              selections.set(id, { ...selection, checked });
            }}
            onSelect={(index) => {
              selections.set(id, {
                ...selection,
                selectedIndex: index,
              });
            }}
          />
        {/if}
      {/each}
    </div>

    <button
      onclick={() => handleDenyAll(data.groups)}
      class="text-text-secondary mb-6 self-start text-sm font-medium hover:underline"
    >
      {$t`Deny All`}
    </button>

    <Button
      onclick={() => handleContinue(data.groups)}
      size="xl"
      class="w-full"
    >
      {$t`Continue`}
    </Button>
  </div>
{/await}
