<script lang="ts">
  import type {
    AttributeConsent,
    AttributeConsentContext,
    AttributeGroup,
    AvailableAttribute,
  } from "$lib/stores/attributeConsent.store";
  import { extractScope } from "$lib/stores/attributeConsent.store";
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

  /** Merge email and verified_email groups for display.
   *  Prefers verified_email options; falls back to email if none available.
   *  The merged group tracks which original attributes each option represents. */
  type MergedOption = {
    display: AvailableAttribute;
    originals: AvailableAttribute[];
  };
  type MergedGroup = {
    name: string;
    options: MergedOption[];
  };

  /** Index options by their scope (everything before the last `:` in the key). */
  const indexByScope = (
    groups: AttributeGroup[],
    name: string,
  ): Map<string | undefined, AvailableAttribute> =>
    new Map(
      groups
        .filter((g) => g.name === name)
        .flatMap((g) => g.options)
        .map((o) => [extractScope(o.key), o]),
    );

  /** Merge email + verified_email into a single "email" group.
   *  Prefers the intersection (verified emails that also have a plain email
   *  in the same scope). Falls back to plain emails if no intersection. */
  const mergeEmailGroups = (
    groups: AttributeGroup[],
  ): MergedGroup | undefined => {
    const emails = indexByScope(groups, "email");
    const verified = indexByScope(groups, "verified_email");

    if (emails.size === 0 && verified.size === 0) {
      return undefined;
    }

    // Intersect: verified emails that also have a matching plain email.
    const intersection: MergedOption[] = [...verified.entries()]
      .filter(([scope]) => emails.has(scope))
      .map(([scope, v]) => ({
        display: v,
        originals: [v, emails.get(scope)!],
      }));

    const options =
      intersection.length > 0
        ? intersection
        : [...emails.values()].map((e) => ({ display: e, originals: [e] }));

    return options.length > 0 ? { name: "email", options } : undefined;
  };

  const mergeGroups = (groups: AttributeGroup[]): MergedGroup[] => {
    const merged: MergedGroup[] = [];

    const emailGroup = mergeEmailGroups(groups);
    if (emailGroup !== undefined) {
      merged.push(emailGroup);
    }

    for (const group of groups) {
      if (group.name === "email" || group.name === "verified_email") {
        continue;
      }
      if (group.options.length > 0) {
        merged.push({
          name: group.name,
          options: group.options.map((o) => ({
            display: o,
            originals: [o],
          })),
        });
      }
    }

    return merged;
  };

  let mergedGroups = $state<MergedGroup[]>([]);
  let selections = $state(
    new Map<string, { checked: boolean; selectedIndex: number }>(),
  );

  $effect(() => {
    context.then((ctx) => {
      mergedGroups = mergeGroups(ctx.groups);
      selections = new Map(
        mergedGroups.map((group) => [
          group.name,
          { checked: true, selectedIndex: 0 },
        ]),
      );
    });
  });

  const handleDenyAll = () => {
    selections = new Map(
      mergedGroups.map((group) => [
        group.name,
        { checked: false, selectedIndex: 0 },
      ]),
    );
  };

  const handleContinue = () => {
    const attributes = mergedGroups
      .filter((group) => selections.get(group.name)?.checked === true)
      .flatMap((group) => {
        const selection = selections.get(group.name)!;
        return group.options[selection.selectedIndex].originals;
      });
    onConsent({ attributes });
  };

  const labelForAttribute = (name: string): string => {
    switch (name) {
      case "email":
        return $t`Email:`;
      case "name":
        return $t`Name:`;
      default:
        return `${name}:`;
    }
  };
</script>

{#await context}
  <!-- Skeleton placeholder while attributes are being resolved -->
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
{:then ctx}
  <div class="flex min-w-0 flex-1 flex-col">
    <AuthorizeHeader origin={ctx.effectiveOrigin} />
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

    <div class="mb-4 flex w-full flex-col gap-1">
      {#each mergedGroups as group (group.name)}
        {@const selection = selections.get(group.name)}
        {#if selection !== undefined}
          <AttributePicker
            label={labelForAttribute(group.name)}
            options={group.options.map((o) => o.display)}
            selectedIndex={selection.selectedIndex}
            checked={selection.checked}
            onCheck={(checked) => {
              selections.set(group.name, { ...selection, checked });
              selections = new Map(selections);
            }}
            onSelect={(index) => {
              selections.set(group.name, {
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
{/await}
