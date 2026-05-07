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
  <div class="mb-3 flex h-9 flex-row items-center">
    <MailIcon class="text-fg-tertiary size-6" />
    <button class="btn btn-secondary btn-sm ms-auto" onclick={onActivate}>
      {$t`Add email`}
    </button>
  </div>
  <h2 class="text-text-primary mb-1 text-base font-semibold">
    {$t`Recovery email`}
  </h2>
  <div class="text-text-tertiary text-sm">
    {$t`Not activated`}
  </div>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="text-text-primary text-xs">
    <Trans>Sign in by sending a signed email from your inbox.</Trans>
  </div>
</section>
