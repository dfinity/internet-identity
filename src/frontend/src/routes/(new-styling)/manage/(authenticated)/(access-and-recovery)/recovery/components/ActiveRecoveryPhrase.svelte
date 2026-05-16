<script lang="ts">
  import { ShieldCheckIcon, LockKeyholeIcon } from "@lucide/svelte";
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
  } & SvelteHTMLElements["section"];

  const {
    recoveryPhrase,
    onReset,
    isCurrentAccessMethod,
    class: className,
    ...props
  }: Props = $props();

  const isProtected = $derived(
    "Protected" in recoveryPhrase.security_settings.protection,
  );
</script>

<section
  {...props}
  class={[
    "bg-bg-primary border-border-secondary flex flex-col rounded-2xl border p-6 not-dark:shadow-sm",
    className,
  ]}
>
  <div class="mb-3 flex h-9 flex-row items-center">
    <div class="relative">
      <ShieldCheckIcon class="text-fg-success-primary size-6" />
      {#if isCurrentAccessMethod}
        <div
          class="bg-bg-success-secondary border-bg-primary absolute top-0 -right-0.25 size-2.5 rounded-full border-2"
        ></div>
      {/if}
    </div>
    <button
      class="btn btn-secondary btn-sm btn-danger ms-auto"
      onclick={onReset}
    >
      {#if isProtected}
        <LockKeyholeIcon class="size-4" />
        <span>{$t`Unlock and reset`}</span>
      {:else}
        <span>{$t`Reset`}</span>
      {/if}
    </button>
  </div>
  <h2 class="text-text-primary mb-1 text-base font-semibold">
    {$t`Recovery phrase`}
  </h2>
  <div class="text-text-tertiary text-sm">
    {$t`Activated`}
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
            label={$t`Currently signed in with this recovery phrase`}
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
  </div>
  <div class="text-text-primary text-xs">
    <Trans>A 24-word phrase you write down and keep offline.</Trans>
  </div>
</section>
