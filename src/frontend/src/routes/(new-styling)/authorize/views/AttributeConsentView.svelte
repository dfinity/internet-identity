<script lang="ts">
  import type {
    AttributeConsent,
    AttributeConsentContext,
  } from "$lib/stores/attributeConsent.store";
  import { SvelteMap } from "svelte/reactivity";
  import {
    extractAttributeName,
    extractScope,
    resolveAttributeGroups,
  } from "$lib/stores/channelHandlers/attributes";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { anonymousActor, backendCanisterConfig } from "$lib/globals";
  import { discoverSsoConfig } from "$lib/utils/ssoDiscovery";
  import { throwCanisterError } from "$lib/utils/utils";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { Trans } from "$lib/components/locale";
  import { handleError } from "$lib/components/utils/error";
  import { VerifiedEmailWizard } from "$lib/components/wizards/verifiedEmail";
  import type {
    EmailRecoveryDnsInput,
    EmailRecoverySubmitDkimLeafArg,
  } from "$lib/generated/internet_identity_types";
  import { MailPlusIcon } from "@lucide/svelte";
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

  /** Whether the dapp asked for `email` or `verified_email` in any shape
   *  (scoped or unscoped). Drives the empty-state branch below — without
   *  an email-shaped request there's no useful CTA to offer, so the
   *  empty-set short-circuit in the handler still fires upstream. */
  const isEmailKey = (key: string): boolean => {
    const name = extractAttributeName(key);
    return name === "email" || name === "verified_email";
  };

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

  /** Live group list — initially derived from the resolved context, but
   *  mutated by {@link handleVerifySuccess} when the user verifies an
   *  email from the empty-state pane so a fresh `list_available_attributes`
   *  result can surface as a picker without remounting the view. */
  let displayGroups = $state<MergedGroup[]>([]);

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
    displayGroups = groups;
    return {
      effectiveOrigin: ctx.effectiveOrigin,
      requestedKeys: ctx.requestedKeys,
      emailRequested: ctx.requestedKeys.some(isEmailKey),
    };
  })();

  let showVerifyWizard = $state(false);

  // --- Canister wrappers for the inline verify flow ------------------
  // Same shape as the settings panel: authenticated `prepare_add`, anonymous
  // status / diagnostics / submit / DoH (keyed by nonce, flow-neutral).

  const prepareAddVerifiedEmail = (input: EmailRecoveryDnsInput) =>
    $authenticatedStore.actor
      .verified_email_prepare_add($authenticatedStore.identityNumber, input)
      .then(throwCanisterError);

  const statusEmailRecovery = (nonce: string) =>
    anonymousActor.email_recovery_status(nonce);

  const diagnosticsEmailRecovery = (nonce: string) =>
    anonymousActor.email_recovery_diagnostics(nonce);

  const submitEmailDkimLeaf = async (
    arg: EmailRecoverySubmitDkimLeafArg,
  ): Promise<void> => {
    await throwCanisterError(
      await anonymousActor.email_recovery_submit_dkim_leaf(arg),
    );
  };

  const resolveEmailViaDoh = async (nonce: string): Promise<void> => {
    await throwCanisterError(
      await anonymousActor.email_recovery_resolve_via_doh({ nonce }),
    );
  };

  /** Re-evaluate available attributes after a successful verification and
   *  update {@link displayGroups}. If the canister now exposes the verified
   *  email as an attribute source (Phase 2+ wiring), the empty-state pane
   *  is replaced by the picker; otherwise the user lands back on the same
   *  pane and can fall through with "Skip for now". */
  const refetchGroups = async (requestedKeys: string[]): Promise<void> => {
    try {
      const available = await $authenticatedStore.actor
        .list_available_attributes({
          identity_number: $authenticatedStore.identityNumber,
          attributes: [],
        })
        .then(throwCanisterError);
      const groups = mergeGroups(
        resolveAttributeGroups(requestedKeys, available),
      );
      await Promise.all(ssoDomainsIn(groups).map(discoverSsoName));
      selections.clear();
      for (const group of groups) {
        selections.set(groupId(group), { checked: true, selectedIndex: 0 });
      }
      displayGroups = groups;
    } catch (error) {
      handleError(error);
    }
  };

  const handleVerifySuccess = async (
    _address: string,
    requestedKeys: string[],
  ): Promise<void> => {
    showVerifyWizard = false;
    await refetchGroups(requestedKeys);
  };

  const handleSkip = () => onConsent({ attributes: [] });

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
    {#if displayGroups.length === 0 && data.emailRequested}
      <!-- Empty-state pane: the dapp asked for an email-shaped attribute
           but the user has no source available. Offer an inline
           "Verify an email" affordance; "Skip for now" falls through to
           the same empty-set certification the handler used to short-
           circuit silently. -->
      <div
        class="bg-bg-tertiary text-fg-tertiary mb-4 flex size-14 items-center justify-center rounded-2xl"
      >
        <MailPlusIcon class="size-7" />
      </div>
      <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
        {$t`Add a verified email`}
      </h1>
      <p class="text-text-secondary mb-6 self-start text-sm">
        <Trans>
          This app would like to reach you by email. You can verify an address
          now to share it, or skip and continue without sharing.
        </Trans>
      </p>
      <button
        class="btn btn-primary btn-xl mb-3 w-full"
        onclick={() => (showVerifyWizard = true)}
      >
        {$t`Verify an email`}
      </button>
      <button class="btn btn-tertiary btn-xl w-full" onclick={handleSkip}>
        {$t`Skip for now`}
      </button>
    {:else}
      <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
        {#if variant === "openid"}
          {$t`Review permissions`}
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
        {#each displayGroups as group (groupId(group))}
          <div
            class="flex items-center gap-x-3 overflow-hidden px-3 whitespace-nowrap"
          >
            <span class="size-4 shrink-0"></span>
            <span class="shrink-0 text-sm" data-label
              >{labelForGroup(group)}</span
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
        {#each displayGroups as group (groupId(group))}
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
        onclick={() => handleDenyAll(displayGroups)}
        class="text-text-secondary mb-6 self-start text-sm font-medium hover:underline"
      >
        {$t`Deny all`}
      </button>

      <button
        class="btn btn-primary btn-xl w-full"
        onclick={() => handleContinue(displayGroups)}
      >
        {$t`Continue`}
      </button>
    {/if}
  </div>

  {#if showVerifyWizard}
    <Dialog
      onClose={() => (showVerifyWizard = false)}
      closeOnOutsideClick={false}
    >
      <VerifiedEmailWizard
        prepare={prepareAddVerifiedEmail}
        status={statusEmailRecovery}
        diagnostics={diagnosticsEmailRecovery}
        submitDkimLeaf={submitEmailDkimLeaf}
        resolveViaDoh={resolveEmailViaDoh}
        onSuccess={(address) =>
          handleVerifySuccess(address, data.requestedKeys)}
      />
    </Dialog>
  {/if}
{/await}
