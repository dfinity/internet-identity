<script lang="ts">
  import type {
    AttributeConsent,
    AttributeConsentContext,
    AttributeGroup,
    AvailableAttribute,
  } from "$lib/stores/attributeConsent.store";
  import { extractScope } from "$lib/stores/channelHandlers/attributes";
  import { backendCanisterConfig } from "$lib/globals";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { t } from "$lib/stores/locale.store";
  import AttributePicker from "./AttributePicker.svelte";

  interface Props {
    context: Promise<AttributeConsentContext>;
    variant: "openid" | "normal";
    onConsent: (consent: AttributeConsent) => void;
  }

  const { context, variant, onConsent }: Props = $props();

  type MergedOption = {
    display: AvailableAttribute;
    originals: AvailableAttribute[];
  };
  /** A MergedGroup maps to one AttributePicker row.
   *  Scoped and unscoped requests for the same attribute produce separate
   *  groups so each gets its own row (and label). */
  type MergedGroup = {
    name: string;
    omitScope: boolean;
    options: MergedOption[];
  };

  const groupId = (group: { name: string; omitScope: boolean }): string =>
    `${group.name}:${group.omitScope ? "u" : "s"}`;

  const getProviderName = (key: string): string | undefined => {
    const scope = extractScope(key);
    if (scope === undefined || !scope.startsWith("openid:")) {
      return undefined;
    }
    const issuer = scope.slice("openid:".length);
    return backendCanisterConfig.openid_configs[0]?.find(
      (c) => c.issuer === issuer,
    )?.name;
  };

  /** Merge AttributeGroups by (name, omitScope). Within each bucket, options
   *  are deduped by scope so exact-duplicate requests collapse while keeping
   *  all originals so every form is certified. */
  const dedupeByFormAndName = (
    groups: AttributeGroup[],
  ): Map<
    string,
    {
      name: string;
      omitScope: boolean;
      optionsByScope: Map<string | undefined, MergedOption>;
    }
  > => {
    const result = new Map<
      string,
      {
        name: string;
        omitScope: boolean;
        optionsByScope: Map<string | undefined, MergedOption>;
      }
    >();
    for (const group of groups) {
      if (group.options.length === 0) {
        continue;
      }
      const omitScope = group.options[0].omitScope;
      const id = groupId({ name: group.name, omitScope });
      const entry = result.get(id) ?? {
        name: group.name,
        omitScope,
        optionsByScope: new Map<string | undefined, MergedOption>(),
      };
      for (const option of group.options) {
        const scope = extractScope(option.key);
        const existing = entry.optionsByScope.get(scope);
        if (existing !== undefined) {
          existing.originals.push(option);
        } else {
          entry.optionsByScope.set(scope, {
            display: option,
            originals: [option],
          });
        }
      }
      result.set(id, entry);
    }
    return result;
  };

  const mergeGroups = (groups: AttributeGroup[]): MergedGroup[] => {
    const deduped = dedupeByFormAndName(groups);

    // Pre-scan: figure out which verified_email entries will be folded into
    // a matching email group. We do this before iterating so that the
    // folded-in verified_email is skipped regardless of whether its entry
    // appears before or after the email entry in the request order.
    const foldedVerifiedIds = new Set<string>();
    for (const entry of deduped.values()) {
      if (entry.name !== "email") continue;
      const verifiedId = groupId({
        name: "verified_email",
        omitScope: entry.omitScope,
      });
      if (deduped.has(verifiedId)) {
        foldedVerifiedIds.add(verifiedId);
      }
    }

    // Iterate in request order so the UI preserves the dapp's key order.
    const result: MergedGroup[] = [];
    for (const [id, entry] of deduped) {
      if (entry.name === "verified_email" && foldedVerifiedIds.has(id)) {
        continue;
      }
      if (entry.name === "email") {
        // Merge email + verified_email that share the same form
        // (scoped/unscoped), intersecting by scope — so a matching verified
        // option replaces the plain email for display but both originals get
        // certified.
        const verifiedId = groupId({
          name: "verified_email",
          omitScope: entry.omitScope,
        });
        const verified = deduped.get(verifiedId);
        if (verified !== undefined) {
          const options: MergedOption[] = [];
          for (const [scope, emailOpt] of entry.optionsByScope) {
            const verifiedOpt = verified.optionsByScope.get(scope);
            options.push(
              verifiedOpt !== undefined
                ? {
                    display: verifiedOpt.display,
                    originals: [
                      ...verifiedOpt.originals,
                      ...emailOpt.originals,
                    ],
                  }
                : emailOpt,
            );
          }
          result.push({
            name: "email",
            omitScope: entry.omitScope,
            options,
          });
        } else {
          result.push({
            name: "email",
            omitScope: entry.omitScope,
            options: [...entry.optionsByScope.values()],
          });
        }
      } else {
        result.push({
          name: entry.name,
          omitScope: entry.omitScope,
          options: [...entry.optionsByScope.values()],
        });
      }
    }

    return result;
  };

  let mergedGroups = $state<MergedGroup[]>([]);
  let selections = $state(
    new Map<string, { checked: boolean; selectedIndex: number }>(),
  );
  let effectiveOrigin = $state("");
  // `ready` is flipped only after `mergedGroups`, `selections`, and
  // `effectiveOrigin` have all been set from the resolved context — the UI
  // below gates on it so a click can never submit with default-empty state.
  let ready = $state(false);

  $effect(() => {
    context.then((ctx) => {
      mergedGroups = mergeGroups(ctx.groups);
      selections = new Map(
        mergedGroups.map((group) => [
          groupId(group),
          { checked: true, selectedIndex: 0 },
        ]),
      );
      effectiveOrigin = ctx.effectiveOrigin;
      ready = true;
    });
  });

  const handleDenyAll = () => {
    selections = new Map(
      mergedGroups.map((group) => [
        groupId(group),
        { checked: false, selectedIndex: 0 },
      ]),
    );
  };

  const handleContinue = () => {
    const attributes = mergedGroups
      .filter((group) => selections.get(groupId(group))?.checked === true)
      .flatMap((group) => {
        const selection = selections.get(groupId(group))!;
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

  const labelForGroup = (group: MergedGroup): string => {
    // For scoped single-provider rows, prefix with the provider's name so the
    // user can tell it apart from the matching unscoped row.
    const scopedProvider =
      !group.omitScope && group.options.length === 1
        ? getProviderName(group.options[0].display.key)
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

{#if !ready}
  <!-- Skeleton placeholder while the context promise resolves and state is
       being initialized. Gating on `ready` (instead of `{#await context}`)
       avoids a frame where `{:then}` renders with default-empty state and
       a fast click could submit an empty consent result. -->
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
{:else}
  <div class="flex min-w-0 flex-1 flex-col">
    <AuthorizeHeader origin={effectiveOrigin} />
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

    <div class="mb-4 grid w-full grid-cols-[auto_auto_1fr_auto] gap-y-1">
      {#each mergedGroups as group (groupId(group))}
        {@const id = groupId(group)}
        {@const selection = selections.get(id)}
        {#if selection !== undefined}
          <AttributePicker
            label={labelForGroup(group)}
            options={group.options.map((o) => o.display)}
            selectedIndex={selection.selectedIndex}
            checked={selection.checked}
            onCheck={(checked) => {
              selections.set(id, { ...selection, checked });
              selections = new Map(selections);
            }}
            onSelect={(index) => {
              selections.set(id, {
                ...selection,
                selectedIndex: index,
              });
              selections = new Map(selections);
            }}
          />
        {/if}
      {/each}
    </div>

    <button
      onclick={handleDenyAll}
      class="text-text-secondary mb-6 self-start text-sm font-medium hover:underline"
    >
      {$t`Deny All`}
    </button>

    <Button onclick={handleContinue} size="xl" class="w-full">
      {$t`Continue`}
    </Button>
  </div>
{/if}
