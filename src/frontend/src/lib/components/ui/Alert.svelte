<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import {
    InfoIcon,
    CircleCheckIcon,
    CircleAlertIcon,
    XIcon,
  } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  type Props = HTMLAttributes<HTMLDivElement> & {
    title: string;
    description?: string;
    variant?: "info" | "success" | "warning" | "error" | "loading";
    direction?: "horizontal" | "vertical";
    onClose?: () => void;
  };

  const {
    children,
    class: className,
    title,
    description,
    variant = "info",
    direction = "vertical",
    onClose,
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
    {#if variant === "info"}
      <InfoIcon size="1.25rem" class="text-fg-brand-primary" />
    {:else if variant === "success"}
      <CircleCheckIcon size="1.25rem" class="text-fg-success-primary" />
    {:else if variant === "warning"}
      <CircleAlertIcon size="1.25rem" class="text-fg-warning-primary" />
    {:else if variant === "error"}
      <CircleAlertIcon size="1.25rem" class="text-fg-error-primary" />
    {:else if variant === "loading"}
      <ProgressRing class="text-fg-brand-primary" />
    {/if}
  </div>
  {#if nonNullish(onClose)}
    <Button
      onclick={onClose}
      variant="tertiary"
      size="sm"
      iconOnly
      class={[
        "col-start-3 -m-2 !rounded-full",
        direction === "horizontal" && "row-span-2",
      ]}
    >
      <XIcon size="1.25rem" />
    </Button>
  {/if}
  <div
    class={[
      "flex flex-col gap-1",
      direction === "horizontal"
        ? "col-start-2 row-span-2 row-start-1"
        : "col-span-3",
      direction === "horizontal" && isNullish(onClose) && "col-span-2",
    ]}
  >
    <div class="text-text-primary text-sm font-semibold">
      {title}
    </div>
    {#if nonNullish(description) || nonNullish(children)}
      <div class="flex flex-col gap-3">
        {#if nonNullish(description)}
          <div class="text-text-tertiary text-sm font-medium">
            {description}
          </div>
        {/if}
        {#if nonNullish(children)}
          <div class="flex gap-3">
            {@render children()}
          </div>
        {/if}
      </div>
    {/if}
  </div>
</div>
