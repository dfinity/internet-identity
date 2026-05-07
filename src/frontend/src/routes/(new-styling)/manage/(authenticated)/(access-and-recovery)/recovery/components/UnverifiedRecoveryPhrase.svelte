<script lang="ts">
  import {
    EllipsisVerticalIcon,
    ShieldIcon,
    ShieldCheckIcon,
    Trash2Icon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { SvelteHTMLElements } from "svelte/elements";
  import Select from "$lib/components/ui/Select.svelte";

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
  <div class="mb-3 flex h-9 flex-row items-center">
    <ShieldIcon class="text-fg-warning-primary size-6" />
    <Select
      options={[
        {
          label: $t`Verify`,
          icon: ShieldCheckIcon,
          onClick: onVerify,
        },
        {
          label: $t`Reset`,
          icon: Trash2Icon,
          onClick: onReset,
        },
      ]}
      align="end"
    >
      <button
        class="btn btn-tertiary btn-sm btn-icon ms-auto"
        aria-label={$t`More options`}
      >
        <EllipsisVerticalIcon class="size-5" />
      </button>
    </Select>
  </div>
  <div class="text-text-primary mb-1 text-base font-semibold">
    {$t`Recovery phrase`}
  </div>
  <div class="text-text-tertiary text-sm">
    {$t`Not verified`}
  </div>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="text-text-primary text-xs">
    <Trans>
      You can use it now to recover, but verify you've saved it correctly so you
      can rely on it later.
    </Trans>
  </div>
</section>
