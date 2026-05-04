<script lang="ts">
  import { ShieldCheckIcon, LockKeyholeIcon } from "@lucide/svelte";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
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

  const lastUsedLabel = $derived.by(() => {
    if (isCurrentAccessMethod) {
      return $t`Last used: right now`;
    }
    const ts = recoveryPhrase.last_authentication[0];
    if (ts !== undefined) {
      return $t`Last used: ${$formatRelative(new Date(nanosToMillis(ts)), { style: "long" })}`;
    }
    return $t`Never used`;
  });
  const lastUsedTooltip = $derived.by(() => {
    if (isCurrentAccessMethod) {
      return $t`Currently signed in with this recovery phrase`;
    }
    const ts = recoveryPhrase.last_authentication[0];
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
    <ShieldCheckIcon class="text-fg-success-primary mt-0.5 size-5 shrink-0" />
    <div class="flex flex-col">
      <h2 class="text-text-primary text-base font-semibold">
        {$t`Recovery phrase`}
      </h2>
      <Tooltip label={lastUsedTooltip} direction="up" align="start">
        <p class="text-text-tertiary cursor-default text-sm">
          {lastUsedLabel}
        </p>
      </Tooltip>
    </div>
  </header>
  <div class="mt-5">
    <Tooltip
      label={$t`Locked legacy recovery phrase`}
      description={$t`Requires unlocking before it can be reset.`}
      hidden={!isProtected}
    >
<<<<<<< HEAD
      <button class="btn btn-secondary btn-sm btn-danger" onclick={onReset}>
=======
      <button
        class={[
          "btn btn-secondary btn-sm btn-danger",
          "@max-xl:mt-5",
          "@xl:my-auto",
        ]}
        onclick={onReset}
      >
>>>>>>> f859c0a6 (refactor(fe): remove deprecated Button component (#3837))
        {#if isProtected}
          <LockKeyholeIcon class="size-4" />
          <span>{$t`Unlock and reset`}</span>
        {:else}
          <span>{$t`Reset`}</span>
        {/if}
      </button>
    </Tooltip>
  </div>
</section>
