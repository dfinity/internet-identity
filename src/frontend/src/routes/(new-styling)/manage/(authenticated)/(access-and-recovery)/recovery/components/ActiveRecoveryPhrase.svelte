<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { ShieldCheckIcon } from "@lucide/svelte";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { SvelteHTMLElements } from "svelte/elements";
  import { nanosToMillis } from "$lib/utils/time";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { AuthnMethodData } from "$lib/generated/internet_identity_types";

  type Props = {
    recoveryPhrase: AuthnMethodData;
    onReset: () => void;
    isCurrentAccessMethod: boolean;
  } & SvelteHTMLElements["div"];

  const {
    recoveryPhrase,
    onReset,
    isCurrentAccessMethod,
    class: className,
    ...props
  }: Props = $props();
</script>

<div class="@container">
  <div
    {...props}
    class={[
      "flex rounded-2xl border p-6",
      "bg-bg-primary border-border-secondary not-dark:shadow-sm",
      "@max-xl:flex-col",
      "@xl:flex-row",
      className,
    ]}
  >
    <ShieldCheckIcon
      class={["text-fg-success-primary size-5 shrink-0", "@xl:mt-0.5"]}
    />
    <div class={["flex flex-col", "@max-xl:mt-3", "@xl:mx-3"]}>
      <div class="text-text-primary text-base font-semibold">
        {$t`Recovery phrase activated`}
      </div>
      <div class="text-text-tertiary text-sm">
        <Trans>It can be used at any time to recover your identity.</Trans>
      </div>
    </div>
    <div class={["border-border-tertiary my-5 border-t", "@xl:hidden"]}></div>
    <div class={["flex flex-col gap-1", "@xl:my-auto @xl:ms-auto @xl:me-20"]}>
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
        {:else if recoveryPhrase.last_authentication[0] !== undefined}
          {@const date = new Date(
            nanosToMillis(recoveryPhrase.last_authentication[0]),
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
    <Button
      onclick={onReset}
      danger
      variant="secondary"
      size="sm"
      class={["@max-xl:mt-5", "@xl:my-auto"]}
    >
      {$t`Reset`}
    </Button>
  </div>
</div>
