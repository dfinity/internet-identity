<script lang="ts">
  import { EllipsisVerticalIcon, PencilIcon, Trash2Icon } from "@lucide/svelte";
  import { nonNullish } from "@dfinity/utils";
  import { nanosToMillis } from "$lib/utils/time";
  import Select from "$lib/components/ui/Select.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { AuthnMethodData } from "$lib/generated/internet_identity_types";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import { getAuthnMethodAlias } from "$lib/utils/webAuthn";
  import { isLegacyAuthnMethod } from "$lib/utils/accessMethods";

  interface Props {
    passkey: AuthnMethodData;
    onRename?: () => void;
    onRemove?: () => void;
    inUse?: boolean;
  }

  const { passkey, onRename, onRemove, inUse }: Props = $props();

  const alias = $derived(getAuthnMethodAlias(passkey));
  const options = $derived([
    ...(nonNullish(onRename)
      ? [
          {
            label: $t`Rename`,
            icon: PencilIcon,
            onClick: onRename,
          },
        ]
      : []),
    ...(nonNullish(onRemove)
      ? [
          {
            label: $t`Remove`,
            icon: Trash2Icon,
            onClick: onRemove,
          },
        ]
      : []),
  ]);
  const isLegacy = $derived(isLegacyAuthnMethod(passkey));
</script>

<div class={[isLegacy && "opacity-50"]}>
  <div class="mb-3 flex h-9 flex-row items-center">
    <div class="relative">
      <PasskeyIcon class="text-fg-primary size-6" />
      {#if inUse}
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
    {alias}
  </div>
  <div class="text-text-tertiary text-sm">
    {$t`Passkey`}
  </div>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="mb-4 flex flex-row">
    <div class="flex flex-1 flex-col gap-1">
      <div class="text-text-primary text-xs font-semibold">
        {$t`Last used`}
      </div>
      <div class="text-text-primary cursor-default text-xs">
        {#if inUse}
          <Tooltip
            label={$t`Currently signed in with this passkey`}
            direction="up"
            align="start"
          >
            <span>{$t`Right now`}</span>
          </Tooltip>
        {:else if nonNullish(passkey.last_authentication[0])}
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
    {$t`Stored securely on your device, in your password manager, or on a security key.`}
  </div>
</div>
