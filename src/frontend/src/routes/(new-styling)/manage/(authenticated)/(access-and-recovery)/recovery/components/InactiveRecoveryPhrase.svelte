<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { ShieldOffIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { SvelteHTMLElements } from "svelte/elements";

  type Props = {
    onActivate: () => void;
  } & SvelteHTMLElements["section"];

  const { onActivate, class: className, ...props }: Props = $props();
</script>

<div class="@container">
  <section
    {...props}
    class={[
      "flex rounded-2xl border p-6",
      "bg-bg-error-primary border-fg-error-primary",
      "@max-lg:flex-col",
      "@lg:flex-row",
      className,
    ]}
  >
    <ShieldOffIcon
      class={["text-fg-error-primary size-5 shrink-0", "@lg:mt-0.5"]}
    />
    <header class={["flex flex-col", "@max-lg:mt-3 @max-lg:mb-5", "@lg:mx-3"]}>
      <h2 class="text-text-primary text-base font-semibold">
        {$t`Recovery phrase not activated`}
      </h2>
      <p class="text-text-tertiary text-sm">
        <Trans>
          Activate it so that you can recover your identity at any time.
        </Trans>
      </p>
    </header>
    <Button onclick={onActivate} size="sm" class="@lg:my-auto @lg:ms-auto">
      {$t`Activate`}
    </Button>
  </section>
</div>
