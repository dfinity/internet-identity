<script lang="ts">
  import type { AvailableAttribute } from "$lib/stores/attributeConsent.store";
  import { extractScope } from "$lib/stores/attributeConsent.store";
  import { backendCanisterConfig } from "$lib/globals";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import { CheckIcon, XIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    label: string;
    options: AvailableAttribute[];
    selectedIndex: number;
    checked: boolean;
    onCheck: (checked: boolean) => void;
    onSelect: (index: number) => void;
  }

  const { label, options, selectedIndex, checked, onCheck, onSelect }: Props =
    $props();

  let isOpen = $state(false);

  const getProvider = (
    key: string,
  ): { name: string; logo: string } | undefined => {
    const scope = extractScope(key);
    if (scope === undefined || !scope.startsWith("openid:")) {
      return undefined;
    }
    const issuer = scope.slice("openid:".length);
    const config = backendCanisterConfig.openid_configs[0]?.find(
      (c) => c.issuer === issuer,
    );
    if (config === undefined) {
      return undefined;
    }
    return { name: config.name, logo: config.logo };
  };

  const selectedProvider = $derived(getProvider(options[selectedIndex].key));
</script>

<fieldset
  class="border-border-secondary min-w-0 overflow-hidden rounded-lg border"
>
  <div class="flex items-center gap-3 p-3">
    <Checkbox
      {checked}
      onchange={() => onCheck(!checked)}
      size="sm"
      aria-label={$t`Share ${label}`}
    />
    <span class="text-text-secondary text-sm">{label}</span>
    {#if selectedProvider !== undefined}
      <span
        class="flex shrink-0 items-center justify-center [&_svg]:size-4"
        aria-hidden="true"
      >
        {@html selectedProvider.logo}
      </span>
    {/if}
    <span
      class="text-text-primary min-w-0 flex-1 overflow-hidden text-sm font-medium text-ellipsis whitespace-nowrap"
    >
      {options[selectedIndex].displayValue}
    </span>
    {#if options.length > 1}
      {#if isOpen}
        <button
          onclick={() => (isOpen = false)}
          class="btn btn-tertiary btn-sm btn-icon size-6! shrink-0"
          aria-label={$t`Close`}
          aria-expanded="true"
        >
          <XIcon class="size-4" />
        </button>
      {:else}
        <button
          onclick={() => (isOpen = true)}
          class="text-text-secondary shrink-0 text-sm font-medium hover:underline"
          aria-expanded="false"
        >
          {$t`Change`}
        </button>
      {/if}
    {/if}
  </div>

  {#if isOpen && options.length > 1}
    <div
      class="border-border-secondary group/list flex flex-col border-t"
      role="listbox"
      aria-label={label}
    >
      {#each options as option, index}
        {@const provider = getProvider(option.key)}
        <button
          onclick={() => {
            onSelect(index);
            isOpen = false;
          }}
          class={[
            "flex flex-row items-center gap-3 p-3 text-start",
            index === selectedIndex
              ? "bg-bg-active hover:bg-bg-active! group-hover/list:bg-transparent"
              : "hover:bg-bg-primary_hover",
          ]}
          role="option"
          aria-selected={index === selectedIndex}
        >
          <span class="text-text-primary text-sm font-medium">
            {option.displayValue}
          </span>
          {#if provider !== undefined}
            <span class="text-text-tertiary text-sm">
              {provider.name}
            </span>
          {/if}
          {#if index === selectedIndex}
            <CheckIcon
              class="text-fg-tertiary ms-auto me-1 size-4 shrink-0"
              aria-hidden="true"
            />
          {/if}
        </button>
      {/each}
    </div>
  {/if}
</fieldset>
