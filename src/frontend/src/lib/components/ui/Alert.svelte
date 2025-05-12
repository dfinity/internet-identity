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
    "grid grid-cols-[auto_1fr_auto] grid-rows-[auto_auto] items-start gap-4 rounded-xl border p-4",
    "bg-gray-light-25 border-gray-light-200",
    "dark:bg-gray-dark-800 dark:border-gray-light-700",
    className,
  ]}
>
  <div class={[direction === "horizontal" && "row-span-2"]}>
    {#if variant === "info"}
      <InfoIcon
        size="1.25rem"
        class={["text-gray-light-600", "dark:text-gray-light-500"]}
      />
    {:else if variant === "success"}
      <CircleCheckIcon
        size="1.25rem"
        class={["text-success-600", "dark:text-success-500"]}
      />
    {:else if variant === "warning"}
      <CircleAlertIcon
        size="1.25rem"
        class={["text-warning-600", "dark:text-warning-500"]}
      />
    {:else if variant === "error"}
      <CircleAlertIcon
        size="1.25rem"
        class={["text-error-600", "dark:text-error-500"]}
      />
    {:else if variant === "loading"}
      <ProgressRing
        value={null}
        class={["text-gray-light-600", "dark:text-gray-light-500"]}
      />
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
    <div
      class={[
        "text-sm font-semibold",
        "text-gray-light-900",
        "dark:text-gray-dark-25",
      ]}
    >
      {title}
    </div>
    {#if nonNullish(description) || nonNullish(children)}
      <div class="flex flex-col gap-3">
        {#if nonNullish(description)}
          <div
            class={[
              "text-sm font-medium",
              "text-gray-light-600",
              "dark:text-gray-dark-50",
            ]}
          >
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
