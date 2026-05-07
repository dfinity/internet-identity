<script lang="ts">
  import {
    EllipsisVerticalIcon,
    MailCheckIcon,
    PencilIcon,
    Trash2Icon,
  } from "@lucide/svelte";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { SvelteHTMLElements } from "svelte/elements";
  import { nanosToMillis } from "$lib/utils/time";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import Select from "$lib/components/ui/Select.svelte";
  import type { EmailRecoveryCredential } from "$lib/generated/internet_identity_types";

  type Props = {
    credential: EmailRecoveryCredential;
    onReplace: () => void;
    onRemove: () => void;
  } & SvelteHTMLElements["section"];

  const {
    credential,
    onReplace,
    onRemove,
    class: className,
    ...props
  }: Props = $props();
</script>

<section
  {...props}
  class={[
    "bg-bg-primary border-border-secondary flex flex-col rounded-2xl border p-6 not-dark:shadow-sm",
    className,
  ]}
>
  <div class="mb-3 flex h-9 flex-row items-center">
    <MailCheckIcon class="text-fg-success-primary size-6" />
    <Select
      options={[
        {
          label: $t`Replace`,
          icon: PencilIcon,
          onClick: onReplace,
        },
        {
          label: $t`Remove`,
          icon: Trash2Icon,
          onClick: onRemove,
        },
      ]}
      align="end"
    >
      <button
        class="btn btn-tertiary btn-sm btn-icon ms-auto"
        aria-label={$t`More options`}
      >
        <EllipsisVerticalIcon class="size-5" />
      </button>
    </Select>
  </div>
  <div class="text-text-primary mb-1 text-base font-semibold">
    {$t`Recovery email`}
  </div>
  <div class="text-text-tertiary truncate text-sm">{credential.address}</div>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="mb-4 flex flex-row">
    <div class="flex flex-1 flex-col gap-1">
      <div class="text-text-primary text-xs font-semibold">
        {$t`Last used`}
      </div>
      <div class="text-text-primary cursor-default text-xs">
        {#if credential.last_used[0] !== undefined}
          {@const date = new Date(nanosToMillis(credential.last_used[0]))}
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
    <Trans>Sign in by sending a signed email from your inbox.</Trans>
  </div>
</section>
