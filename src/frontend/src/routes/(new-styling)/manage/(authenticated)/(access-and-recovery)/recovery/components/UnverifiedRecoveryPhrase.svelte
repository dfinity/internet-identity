<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { ShieldIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { SvelteHTMLElements } from "svelte/elements";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";

  type Props = {
    onReset: () => void;
    onVerify: () => void;
  } & SvelteHTMLElements["section"];

  const { onReset, onVerify, class: className, ...props }: Props = $props();
</script>

<div class="@container">
  <section
    {...props}
    class={[
      "flex flex-col rounded-2xl border p-6",
      "bg-bg-warning-primary border-fg-warning-primary not-dark:shadow-sm",
      "@xl:flex-row",
      className,
    ]}
  >
    <ShieldIcon
      class={["text-fg-warning-primary size-5 shrink-0", "@xl:mt-0.5"]}
    />
    <header class={["flex flex-col", "@max-xl:mt-3", "@xl:mx-3"]}>
      <h2 class="text-text-primary text-base font-semibold">
        {$t`Recovery phrase not verified`}
      </h2>
      <p class="text-text-tertiary text-sm">
        <Trans>
          You can recover your identity now, verify you saved it correctly.
        </Trans>
      </p>
    </header>
    <hr class={["border-border-tertiary my-5 border-t", "@xl:hidden"]} />
    <dl class={["flex flex-col gap-1", "@xl:my-auto @xl:ms-auto @xl:me-20"]}>
      <dt class="text-text-primary text-xs font-semibold">
        {$t`Last used`}
      </dt>
      <dd class="text-text-primary cursor-default text-xs">
        <Tooltip label={$t`Has not been used yet`} direction="up" align="start">
          <span>{$t`n/a`}</span>
        </Tooltip>
      </dd>
    </dl>
    <div
      class={[
        "flex flex-col gap-1.5",
        "@max-xl:mt-5 ",
        "@xl:my-auto @xl:ms-auto @xl:flex-row @xl:gap-4",
      ]}
    >
      <Button onclick={onReset} danger variant="secondary" size="sm">
        {$t`Reset`}
      </Button>
      <Button onclick={onVerify} size="sm">
        {$t`Verify`}
      </Button>
    </div>
  </section>
</div>
