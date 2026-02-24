<script lang="ts">
  import { EllipsisVerticalIcon, PencilIcon, Trash2Icon } from "@lucide/svelte";
  import { nanosToMillis } from "$lib/utils/time";
  import Select from "$lib/components/ui/Select.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { AuthnMethodData } from "$lib/generated/internet_identity_types";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import { aaguidToString, getAuthnMethodAlias } from "$lib/utils/webAuthn";
  import { onMount } from "svelte";
  import type { Provider } from "$lib/assets/aaguid";
  import { getMetadataString } from "$lib/utils/openID";
  import { getPrimaryOrigin } from "$lib/globals";

  interface Props {
    passkey: AuthnMethodData;
    onRename?: () => void;
    onRemove?: () => void;
    isCurrentAccessMethod?: boolean;
  }

  const { passkey, onRename, onRemove, isCurrentAccessMethod }: Props =
    $props();

  let knownProviders = $state<Record<string, Provider>>({});

  const alias = $derived.by(() => {
    const value = getAuthnMethodAlias(passkey);
    if (value.length === 0) {
      return;
    }
    return value;
  });
  const aaguid = $derived.by(() => {
    if (!("WebAuthn" in passkey.authn_method)) {
      return;
    }
    const [value] = passkey.authn_method.WebAuthn.aaguid;
    if (value === undefined) {
      return;
    }
    return aaguidToString(new Uint8Array(value));
  });
  const provider = $derived(
    knownProviders !== undefined && aaguid !== undefined
      ? knownProviders[aaguid]
      : undefined,
  );
  const options = $derived([
    ...(onRename !== undefined
      ? [
          {
            label: $t`Rename`,
            icon: PencilIcon,
            onClick: onRename,
          },
        ]
      : []),
    ...(onRemove !== undefined
      ? [
          {
            label: $t`Remove`,
            icon: Trash2Icon,
            onClick: onRemove,
          },
        ]
      : []),
  ]);
  const isLegacy = $derived.by(() => {
    const primaryOrigin = getPrimaryOrigin();
    const origin = getMetadataString(passkey.metadata, "origin");
    if (primaryOrigin === undefined || origin === undefined) {
      return;
    }
    return origin !== primaryOrigin;
  });

  onMount(() => {
    // Lazy load known providers data
    import("$lib/assets/aaguid").then(
      (data) => (knownProviders = data.default),
    );
  });
</script>

<div class={[isLegacy && "opacity-50"]}>
  <div class="mb-3 flex h-9 flex-row items-center">
    <div class="relative">
      <PasskeyIcon class="text-fg-primary size-6" />
      {#if isCurrentAccessMethod}
        <div
          class="bg-bg-success-secondary border-bg-primary absolute top-0 -right-0.25 size-2.5 rounded-full border-2"
        ></div>
      {/if}
    </div>
    {#if options.length > 0 && !isLegacy}
      <Select {options} align="end">
        <Button
          variant="tertiary"
          size="sm"
          iconOnly
          class="ml-auto"
          aria-label={$t`More options`}
        >
          <EllipsisVerticalIcon class="size-5" />
        </Button>
      </Select>
    {/if}
  </div>
  <div class="text-text-primary mb-1 text-base font-semibold">
    {#if alias === undefined}
      {#if provider === undefined}
        {$t`Unknown`}
      {:else}
        {provider.name}
      {/if}
    {:else}
      {alias}
    {/if}
  </div>
  <div class="text-text-tertiary text-sm">
    {#if alias === undefined || provider === undefined}
      {$t`Passkey`}
    {:else}
      {$t`Passkey (${provider.name})`}
    {/if}
  </div>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="mb-4 flex flex-row">
    <div class="flex flex-1 flex-col gap-1">
      <div class="text-text-primary text-xs font-semibold">
        {$t`Last used`}
      </div>
      <div class="text-text-primary cursor-default text-xs">
        {#if isCurrentAccessMethod}
          <Tooltip
            label={$t`Currently signed in with this passkey`}
            direction="up"
            align="start"
          >
            <span>{$t`Right now`}</span>
          </Tooltip>
        {:else if passkey.last_authentication[0] !== undefined}
          {@const date = new Date(
            nanosToMillis(passkey.last_authentication[0]),
          )}
          <Tooltip
            label={$formatDate(date, {
              timeStyle: "short",
              dateStyle: "medium",
            })}
            direction="up"
            align="start"
          >
            <span>{$formatRelative(date, { style: "long" })}</span>
          </Tooltip>
        {:else}
          <Tooltip
            label={$t`Has not been used yet`}
            direction="up"
            align="start"
          >
            <span>{$t`n/a`}</span>
          </Tooltip>
        {/if}
      </div>
    </div>
  </div>
  <div class="text-text-primary text-xs">
    {#if provider?.type === "cloud"}
      {provider.platform === undefined
        ? $t`Stored in your ${provider.account} account and synced across your devices.`
        : $t`Stored in your ${provider.account} account and synced across your ${provider.platform} devices.`}
    {:else if provider?.type === "os"}
      {$t`Stored and usable only on the ${provider.platform} device it was created on.`}
    {:else if provider?.type === "device"}
      {$t`Kept on a physical key. Authenticate on supported devices via tap/insert.`}
    {:else if provider?.type === "browser"}
      {provider.platform === undefined
        ? $t`Stored and usable only in ${provider.browser} on the device it was created on.`
        : $t`Stored and usable only in ${provider.browser} on the ${provider.platform} device it was created on.`}
    {:else}
      {$t`Stored securely on your device, in your password manager, or on a security key.`}
    {/if}
  </div>
</div>
