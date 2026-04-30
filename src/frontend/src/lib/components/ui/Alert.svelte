<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import {
    InfoIcon,
    CircleCheckIcon,
    CircleAlertIcon,
    XIcon,
  } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";

  type Props = HTMLAttributes<HTMLDivElement> & {
    title: string;
    description?: string;
    variant?: "info" | "success" | "warning" | "error" | "loading";
    direction?: "horizontal" | "vertical";
    onClose?: () => void;
    icon?: import("svelte").Snippet;
  };

  const {
    children,
    class: className,
    title,
    description,
    variant = "info",
    direction = "vertical",
    onClose,
    icon,
    ...props
  }: Props = $props();
</script>

<div
  {...props}
  class={[
    "bg-bg-primary_alt border-border-tertiary grid grid-cols-[auto_1fr_auto] grid-rows-[auto_auto] items-start gap-4 rounded-xl border p-4",
    className,
  ]}
>
  <div class={[direction === "horizontal" && "row-span-2"]}>
    {#if icon}
      {@render icon()}
    {:else if variant === "info"}
      <InfoIcon class="text-fg-brand-primary size-5" />
    {:else if variant === "success"}
      <CircleCheckIcon class="text-fg-success-primary size-5" />
    {:else if variant === "warning"}
      <CircleAlertIcon class="text-fg-warning-primary size-5" />
    {:else if variant === "error"}
      <CircleAlertIcon class="text-fg-error-primary size-5" />
    {:else if variant === "loading"}
      <ProgressRing class="text-fg-brand-primary" />
    {/if}
  </div>
  {#if onClose !== undefined}
    <button
      onclick={onClose}
      class={[
        "btn btn-tertiary text-text-secondary btn-sm btn-icon col-start-3 -m-2 rounded-lg",
        direction === "horizontal" && "row-span-2",
      ]}
    >
      <XIcon class=" size-5" />
      <span>{$t`Close`}</span>
    </button>
  {/if}
  <div
    class={[
      "flex flex-col gap-1",
      direction === "horizontal"
        ? "col-start-2 row-span-2 row-start-1"
        : "col-span-3",
      direction === "horizontal" && onClose === undefined && "col-span-2",
    ]}
  >
    <div class="text-text-primary text-sm font-semibold">
      {title}
    </div>
    {#if description !== undefined || children !== undefined}
      <div class="flex flex-col gap-3">
        {#if description !== undefined}
          <div class="text-text-tertiary text-sm">
            {description}
          </div>
        {/if}
        {#if children !== undefined}
          <div class="flex gap-3">
            {@render children()}
          </div>
        {/if}
      </div>
    {/if}
  </div>
</div>
