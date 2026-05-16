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

<div class="@container">
  <section
    {...props}
    class={[
      "flex rounded-2xl border p-6",
      "bg-bg-primary border-border-secondary not-dark:shadow-sm",
      "@max-lg:flex-col",
      "@lg:flex-row",
      className,
    ]}
  >
    <MailIcon class={["text-fg-tertiary size-5 shrink-0", "@lg:mt-0.5"]} />
    <header class={["flex flex-col", "@max-lg:mt-3 @max-lg:mb-5", "@lg:mx-3"]}>
      <h2 class="text-text-primary text-base font-semibold">
        {$t`Recovery email`}
      </h2>
      <p class="text-text-tertiary text-sm">
        <Trans>
          Receive a sign-in link by sending a signed email from your inbox.
        </Trans>
      </p>
    </header>
    <button
      class="btn btn-primary btn-sm @lg:my-auto @lg:ms-auto"
      onclick={onActivate}
    >
      {$t`Add email`}
    </button>
  </section>
</div>
