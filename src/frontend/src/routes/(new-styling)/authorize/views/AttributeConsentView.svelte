<script lang="ts">
  import { onDestroy } from "svelte";
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
  import { lastSharedEmailsStore } from "$lib/stores/last-shared-emails.store";
  import { anonymousActor, backendCanisterConfig } from "$lib/globals";
  import { discoverSsoConfig } from "$lib/utils/ssoDiscovery";
  import { throwCanisterError } from "$lib/utils/utils";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { Trans } from "$lib/components/locale";
  import { handleError } from "$lib/components/utils/error";
  import { VerifiedEmailWizard } from "$lib/components/wizards/verifiedEmail";
  import type {
    EmailChallengeDnsInput,
    EmailChallengeSubmitDkimLeafArg,
  } from "$lib/generated/internet_identity_types";
  import { MailPlusIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import {
    verifiedEmailConsentFunnel,
    VerifiedEmailConsentEvents,
  } from "$lib/utils/analytics/verifiedEmailConsentFunnel";
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

  // Unscoped email/verified_email only; scoped keys are pinned to a
  // source that the inline verify wizard can't satisfy.
  const isEmailKey = (key: string): boolean => {
    if (extractScope(key) !== undefined) return false;
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
    // No scope = unscoped wire row from `Anchor.verified_emails`.
    const scope = extractScope(key);
    if (scope === undefined) return $t`Verified email`;
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

  // Mutated by handleVerifySuccess after a mid-flow verification so a
  // fresh list_available_attributes can surface as a picker.
  let displayGroups = $state<MergedGroup[]>([]);

  // A group is "unscoped" iff the original request carried no scope —
  // tracked authoritatively by `MergedGroup.omitScope`. The wire keys of
  // a group's options can still be scoped (an unscoped `email` request
  // fans out into `openid:<issuer>:email` options); we must not infer
  // scope from those keys.
  const isUnscopedGroup = (group: MergedGroup): boolean => group.omitScope;

  // Stable partition: unscoped first, then scoped; relative order
  // within each tier preserved so we don't perturb the existing
  // mergeGroups ordering for downstream rows.
  const sortGroupsUnscopedFirst = (groups: MergedGroup[]): MergedGroup[] => [
    ...groups.filter(isUnscopedGroup),
    ...groups.filter((g) => !isUnscopedGroup(g)),
  ];

  // For email-shaped groups, prefer the option whose displayed address
  // case-matches the saved last-shared email; fall back to index 0.
  const indexForSavedEmail = (
    group: MergedGroup,
    savedEmail: string | undefined,
  ): number => {
    if (savedEmail === undefined) return 0;
    if (group.name !== "email" && group.name !== "verified_email") return 0;
    const idx = group.options.findIndex(
      (o) => o.display.displayValue.toLowerCase() === savedEmail.toLowerCase(),
    );
    return idx >= 0 ? idx : 0;
  };

  const prepared = (async () => {
    const ctx = await context;
    const groups = sortGroupsUnscopedFirst(mergeGroups(ctx.groups));
    await Promise.all(ssoDomainsIn(groups).map(discoverSsoName));
    const savedEmail = lastSharedEmailsStore.get(
      $authenticatedStore.identityNumber,
      ctx.effectiveOrigin,
    );
    selections.clear();
    for (const group of groups) {
      selections.set(groupId(group), {
        checked: true,
        selectedIndex: indexForSavedEmail(group, savedEmail),
      });
    }
    displayGroups = groups;
    const emailRequested = ctx.requestedKeys.some(isEmailKey);
    if (emailRequested) {
      verifiedEmailConsentFunnel.init({ variant });
      if (groups.length === 0) {
        verifiedEmailConsentFunnel.trigger(
          VerifiedEmailConsentEvents.EmptyStateShown,
        );
      }
    }
    return {
      effectiveOrigin: ctx.effectiveOrigin,
      requestedKeys: ctx.requestedKeys,
      emailRequested,
      recoveryAddresses: ctx.recoveryAddresses,
      verifiedAddresses: ctx.verifiedAddresses,
      openidAddresses: ctx.openidAddresses,
    };
  })();

  let showVerifyWizard = $state(false);

  const prepareAddVerifiedEmail = (input: EmailChallengeDnsInput) =>
    $authenticatedStore.actor
      .verified_email_prepare_add($authenticatedStore.identityNumber, input)
      .then(throwCanisterError);

  const statusEmailRecovery = (nonce: string) =>
    anonymousActor.email_challenge_status(nonce);

  const diagnosticsEmailRecovery = (nonce: string) =>
    anonymousActor.email_challenge_diagnostics(nonce);

  const submitEmailDkimLeaf = async (
    arg: EmailChallengeSubmitDkimLeafArg,
  ): Promise<void> => {
    await throwCanisterError(
      await anonymousActor.email_challenge_submit_dkim_leaf(arg),
    );
  };

  const resolveEmailViaDoh = async (nonce: string): Promise<void> => {
    await throwCanisterError(
      await anonymousActor.email_challenge_resolve_via_doh({ nonce }),
    );
  };

  // `preferredAddress` pre-selects the matching option in email-shaped
  // groups so the user lands on the address they just verified.
  const refetchGroups = async (
    requestedKeys: string[],
    preferredAddress?: string,
  ): Promise<void> => {
    try {
      const available = await $authenticatedStore.actor
        .list_available_attributes({
          identity_number: $authenticatedStore.identityNumber,
          attributes: [],
        })
        .then(throwCanisterError);
      const groups = sortGroupsUnscopedFirst(
        mergeGroups(resolveAttributeGroups(requestedKeys, available)),
      );
      await Promise.all(ssoDomainsIn(groups).map(discoverSsoName));
      selections.clear();
      for (const group of groups) {
        const preferred =
          preferredAddress !== undefined &&
          (group.name === "email" || group.name === "verified_email")
            ? group.options.findIndex(
                (o) =>
                  o.display.displayValue.toLowerCase() ===
                  preferredAddress.toLowerCase(),
              )
            : -1;
        selections.set(groupId(group), {
          checked: true,
          selectedIndex: preferred >= 0 ? preferred : 0,
        });
      }
      displayGroups = groups;
    } catch (error) {
      handleError(error);
    }
  };

  const handleVerifySuccess = async (
    address: string,
    requestedKeys: string[],
  ): Promise<void> => {
    showVerifyWizard = false;
    await refetchGroups(requestedKeys, address);
  };

  const handleOpenVerifyWizard = () => {
    void prepared.then(({ emailRequested }) => {
      if (emailRequested) {
        verifiedEmailConsentFunnel.trigger(
          VerifiedEmailConsentEvents.VerifyClicked,
        );
      }
    });
    showVerifyWizard = true;
  };

  // Catches the abandon path — user closes the tab / navigates away
  // without skip / deny / continue. Funnel.close() is idempotent so
  // overlapping with the explicit terminal calls above is safe.
  onDestroy(() => verifiedEmailConsentFunnel.close());

  const sourceForKey = (key: string): "unscoped" | "openid" | "sso" => {
    const scope = extractScope(key);
    if (scope === undefined) return "unscoped";
    if (scope.startsWith("openid:")) return "openid";
    if (scope.startsWith("sso:")) return "sso";
    return "unscoped";
  };

  const handleSkip = () => {
    verifiedEmailConsentFunnel.trigger(VerifiedEmailConsentEvents.Skipped);
    verifiedEmailConsentFunnel.close();
    onConsent({ attributes: [] });
  };

  const handleDenyAll = (groups: MergedGroup[]) => {
    verifiedEmailConsentFunnel.trigger(VerifiedEmailConsentEvents.DeniedAll);
    for (const group of groups) {
      selections.set(groupId(group), { checked: false, selectedIndex: 0 });
    }
  };

  const handleContinue = async (groups: MergedGroup[]) => {
    const attributes = groups.flatMap((group) => {
      const selection = selections.get(groupId(group));
      if (selection === undefined || !selection.checked) return [];
      return group.options[selection.selectedIndex].originals;
    });
    const sharedEmailAttr = attributes.find((attr) => {
      const name = extractAttributeName(attr.key);
      return name === "email" || name === "verified_email";
    });
    if (sharedEmailAttr !== undefined) {
      verifiedEmailConsentFunnel.trigger(VerifiedEmailConsentEvents.Shared, {
        source: sourceForKey(sharedEmailAttr.key),
      });
      const { effectiveOrigin } = await prepared;
      lastSharedEmailsStore.set(
        $authenticatedStore.identityNumber,
        effectiveOrigin,
        sharedEmailAttr.displayValue,
      );
    }
    verifiedEmailConsentFunnel.close();
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
      <div
        class="bg-bg-tertiary text-fg-tertiary mb-4 flex size-14 items-center justify-center rounded-2xl"
      >
        <MailPlusIcon class="size-7" />
      </div>
      <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
        {$t`Associate an email address with your Internet Identity`}
      </h1>
      <p class="text-text-secondary mb-6 self-start text-sm">
        <Trans>
          This app is requesting your email address. You can verify your email
          address now and share it, or skip and continue without sharing.
        </Trans>
      </p>
      <button
        class="btn btn-primary btn-xl mb-3 w-full"
        onclick={handleOpenVerifyWizard}
      >
        {$t`Verify an email address`}
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
            {#if group.options.length > 1 || group.name === "email" || group.name === "verified_email"}
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
                id: `${o.display.key}|${o.display.displayValue}`,
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
              onVerifyNew={(group.name === "email" ||
                group.name === "verified_email") &&
              isUnscopedGroup(group)
                ? handleOpenVerifyWizard
                : undefined}
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
        recoveryAddresses={data.recoveryAddresses}
        verifiedAddresses={data.verifiedAddresses}
        openidAddresses={data.openidAddresses}
        onSuccess={(address) =>
          handleVerifySuccess(address, data.requestedKeys)}
      />
    </Dialog>
  {/if}
{/await}
