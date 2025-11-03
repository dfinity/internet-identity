<script lang="ts">
  import { getMetadataString, openIdLogo, openIdName } from "$lib/utils/openID";
  import { EllipsisVerticalIcon, Link2OffIcon } from "@lucide/svelte";
  import { nonNullish } from "@dfinity/utils";
  import { nanosToMillis } from "$lib/utils/time";
  import Select from "$lib/components/ui/Select.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { OpenIdCredential } from "$lib/generated/internet_identity_types";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";

  interface Props {
    credential: OpenIdCredential;
    onUnlink?: () => void;
    inUse?: boolean;
  }

  const { credential, onUnlink, inUse }: Props = $props();

  const name = $derived(openIdName(credential.iss, credential.metadata));
  const email = $derived(getMetadataString(credential.metadata, "email"));
  const logo = $derived(openIdLogo(credential.iss, credential.metadata));
  const options = $derived(
    nonNullish(onUnlink)
      ? [
          {
            label: $t`Unlink`,
            icon: Link2OffIcon,
            onClick: onUnlink,
          },
        ]
      : [],
  );
</script>

<div class="mb-3 flex h-9 flex-row items-center">
  {#if nonNullish(logo)}
    <div class="text-fg-primary relative size-6">
      {@html logo}
      {#if inUse}
        <div
          class="bg-bg-success-secondary border-bg-primary absolute -top-0.25 -right-0.5 size-2.5 rounded-full border-2"
        ></div>
      {/if}
    </div>
  {/if}
  {#if options.length > 0}
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
  {$t`${name} account`}
</div>
<div class="text-text-tertiary text-sm">
  {email ?? $t`Hidden email`}
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
          label={$t`Currently signed in with this account`}
          direction="up"
          align="start"
        >
          <span>{$t`Right now`}</span>
        </Tooltip>
      {:else if nonNullish(credential.last_usage_timestamp[0])}
        {@const date = new Date(
          nanosToMillis(credential.last_usage_timestamp[0]),
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
        <Tooltip label={$t`Has not been used yet`} direction="up" align="start">
          <span>{$t`n/a`}</span>
        </Tooltip>
      {/if}
    </div>
  </div>
</div>
<div class="text-text-primary text-xs">
  {$t`Sign in with your ${name} account from any device.`}
</div>
