<script lang="ts">
  import { MailCheckIcon } from "@lucide/svelte";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import type { SvelteHTMLElements } from "svelte/elements";
  import { nanosToMillis } from "$lib/utils/time";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
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

  const lastUsedLabel = $derived.by(() => {
    const ts = credential.last_used[0];
    if (ts !== undefined) {
      return $t`Last used: ${$formatRelative(new Date(nanosToMillis(ts)), { style: "long" })}`;
    }
    return $t`Never used`;
  });
  const lastUsedTooltip = $derived.by(() => {
    const ts = credential.last_used[0];
    if (ts !== undefined) {
      return $formatDate(new Date(nanosToMillis(ts)), {
        timeStyle: "short",
        dateStyle: "medium",
      });
    }
    return $t`Has not been used yet`;
  });
</script>

<section
  {...props}
  class={[
    "bg-bg-primary border-border-secondary flex flex-col rounded-2xl border p-6 not-dark:shadow-sm",
    className,
  ]}
>
  <header class="flex flex-row gap-3">
    <MailCheckIcon class="text-fg-success-primary mt-0.5 size-5 shrink-0" />
    <div class="flex min-w-0 flex-col">
      <h2 class="text-text-primary text-base font-semibold">
        {$t`Recovery email`}
      </h2>
      <p class="text-text-tertiary truncate text-sm">{credential.address}</p>
      <Tooltip label={lastUsedTooltip} direction="up" align="start">
        <p class="text-text-tertiary cursor-default text-xs">
          {lastUsedLabel}
        </p>
      </Tooltip>
    </div>
  </header>
  <div class="mt-5 flex flex-row gap-2">
    <button class="btn btn-secondary btn-sm" onclick={onReplace}>
      {$t`Replace`}
    </button>
    <button class="btn btn-secondary btn-sm btn-danger" onclick={onRemove}>
      {$t`Remove`}
    </button>
  </div>
</section>
