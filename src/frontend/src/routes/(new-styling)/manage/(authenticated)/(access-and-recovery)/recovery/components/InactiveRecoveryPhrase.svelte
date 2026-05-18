<script lang="ts">
  import { ShieldOffIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { SvelteHTMLElements } from "svelte/elements";

  type Props = {
    onActivate: () => void;
  } & SvelteHTMLElements["section"];

  const { onActivate, class: className, ...props }: Props = $props();
</script>

<section
  {...props}
  class={[
    "bg-bg-primary border-border-secondary flex flex-col rounded-2xl border p-6 not-dark:shadow-sm",
    className,
  ]}
>
  <div class="mb-3 flex h-9 flex-row items-center">
    <ShieldOffIcon class="text-fg-tertiary size-6" />
    <button
      class="btn btn-primary btn-sm ms-auto"
      onclick={onActivate}
      aria-label={$t`Activate recovery phrase`}
    >
      {$t`Activate`}
    </button>
  </div>
  <h2 class="text-text-primary mb-1 text-base font-semibold">
    {$t`Recovery phrase`}
  </h2>
  <div class="text-text-tertiary text-sm">
    {$t`Not activated`}
  </div>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="text-text-primary flex flex-col gap-3 text-xs">
    <p>
      <Trans>A 24-word phrase you write down and keep offline.</Trans>
    </p>
    <p>
      <Trans>
        The most reliable backup. Stored entirely by you, on paper. It does not
        depend on any account, service, or provider, and only you can use, lose,
        or replace it.
      </Trans>
    </p>
  </div>
</section>
