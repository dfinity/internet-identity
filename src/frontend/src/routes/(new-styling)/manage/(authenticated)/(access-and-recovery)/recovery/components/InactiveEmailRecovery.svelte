<script lang="ts">
  import { MailIcon } from "@lucide/svelte";
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
  <header class="flex flex-row gap-3">
    <MailIcon class="text-fg-tertiary mt-0.5 size-5 shrink-0" />
    <div class="flex flex-col">
      <h2 class="text-text-primary text-base font-semibold">
        {$t`Recovery email`}
      </h2>
      <p class="text-text-tertiary text-sm">
        <Trans>Not activated.</Trans>
      </p>
    </div>
  </header>
  <div class="mt-5">
    <button class="btn btn-primary btn-sm" onclick={onActivate}>
      {$t`Add email`}
    </button>
  </div>
</section>
