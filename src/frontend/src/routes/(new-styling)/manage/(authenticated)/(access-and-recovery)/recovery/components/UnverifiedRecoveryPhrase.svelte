<script lang="ts">
  import { ShieldIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { SvelteHTMLElements } from "svelte/elements";

  type Props = {
    onReset: () => void;
    onVerify: () => void;
  } & SvelteHTMLElements["section"];

  const { onReset, onVerify, class: className, ...props }: Props = $props();
</script>

<section
  {...props}
  class={[
    "bg-bg-warning-primary border-fg-warning-primary flex flex-col rounded-2xl border p-6 not-dark:shadow-sm",
    className,
  ]}
>
  <header class="flex flex-row gap-3">
    <ShieldIcon class="text-fg-warning-primary mt-0.5 size-5 shrink-0" />
    <div class="flex flex-col">
      <h2 class="text-text-primary text-base font-semibold">
        {$t`Recovery phrase not verified`}
      </h2>
      <p class="text-text-tertiary text-sm">
        <Trans>Verify you saved it correctly.</Trans>
      </p>
    </div>
  </header>
  <div class="mt-5 flex flex-row gap-2">
    <button class="btn btn-primary btn-sm" onclick={onVerify}>
      {$t`Verify`}
    </button>
    <button class="btn btn-secondary btn-sm btn-danger" onclick={onReset}>
      {$t`Reset`}
    </button>
  </div>
</section>
