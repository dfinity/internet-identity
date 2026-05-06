<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { MailCheckIcon } from "@lucide/svelte";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import type { SvelteHTMLElements } from "svelte/elements";
  import { nanosToMillis } from "$lib/utils/time";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { EmailRecoveryCredential } from "$lib/generated/internet_identity_types";

  type Props = {
    credential: EmailRecoveryCredential;
    onReplace: () => void;
    onRemove: () => void;
  } & SvelteHTMLElements["section"];

  const {
    credential,
    onReplace,
    onRemove,
    class: className,
    ...props
  }: Props = $props();
</script>

<div class="@container">
  <section
    {...props}
    class={[
      "flex flex-col rounded-2xl border p-6",
      "bg-bg-primary border-border-secondary not-dark:shadow-sm",
      "@xl:flex-row",
      className,
    ]}
  >
    <MailCheckIcon
      class={["text-fg-success-primary size-5 shrink-0", "@xl:mt-0.5"]}
    />
    <header class={["flex flex-col", "@max-xl:mt-3", "@xl:mx-3"]}>
      <h2 class="text-text-primary text-base font-semibold">
        {$t`Recovery email active`}
      </h2>
      <p class="text-text-tertiary text-sm break-all">{credential.address}</p>
    </header>
    <hr class={["border-border-tertiary my-5 border-t", "@xl:hidden"]} />
    <dl class={["flex flex-col gap-1", "@xl:my-auto @xl:ms-auto @xl:me-20"]}>
      <dt class="text-text-primary text-xs font-semibold">
        {$t`Last used`}
      </dt>
      <dd class="text-text-primary cursor-default text-xs">
        {#if credential.last_used[0] !== undefined}
          {@const date = new Date(nanosToMillis(credential.last_used[0]))}
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
      </dd>
    </dl>
    <div class="flex flex-row gap-2 @max-xl:mt-5 @xl:my-auto">
      <Button onclick={onReplace} variant="secondary" size="sm">
        {$t`Replace`}
      </Button>
      <Button onclick={onRemove} danger variant="secondary" size="sm">
        {$t`Remove`}
      </Button>
    </div>
  </section>
</div>
