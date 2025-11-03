<script lang="ts" generics="T = number">
  import type { Component } from "svelte";
  import type { HTMLAttributes } from "svelte/elements";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";

  type Option<T> = {
    value?: T;
    label: string;
    icon?: Component;
    selected?: boolean;
    onClick?: () => void;
  };
  type Direction = "up" | "right" | "down" | "left";
  type Align = "start" | "center" | "end";

  type Props = HTMLAttributes<HTMLElement> & {
    options: Option<T>[];
    onChange?: (value: T) => void;
    direction?: Direction;
    align?: Align;
    distance?: string;
  };
  let {
    children,
    class: className,
    options,
    onChange,
    direction = "down",
    align = "start",
    distance = "0px",
    ...props
  }: Props = $props();

  let wrapperRef = $state<HTMLElement>();
  let isOpen = $state(false);

  const childrenRef = $derived(
    wrapperRef?.firstElementChild as HTMLElement | undefined,
  );

  const handleClick = (option: Option<T>, index: number) => {
    isOpen = false;
    option.onClick?.();
    onChange?.((option.value ?? index) as T);
  };

  $effect(() => {
    if (isNullish(childrenRef)) {
      return;
    }
    const listener = () => (isOpen = true);
    childrenRef?.addEventListener("click", listener);
    return () => childrenRef.removeEventListener("click", listener);
  });
</script>

<div bind:this={wrapperRef} class="contents">
  {@render children?.()}
</div>

{#if isOpen && nonNullish(childrenRef)}
  <Popover
    {...props}
    anchor={childrenRef}
    {direction}
    {align}
    {distance}
    responsive={false}
    onClose={() => (isOpen = false)}
    class={["!w-max !p-1.5 !shadow-lg", className]}
  >
    <ul class="flex flex-col">
      {#each options as option, index}
        <li class="contents">
          <Button
            onclick={() => handleClick(option, index)}
            variant="tertiary"
            class={[
              "justify-start gap-2.5 !px-3 text-start",
              option.selected && "[ul:not(:hover)_&]:bg-bg-primary_hover",
            ]}
          >
            {#if nonNullish(option.icon)}
              {@const Icon = option.icon}
              <div
                class="text-fg-quaternary dark:text-fg-tertiary [&_svg]:size-4"
              >
                <Icon />
              </div>
            {/if}
            <span>{option.label}</span>
          </Button>
        </li>
      {/each}
    </ul>
  </Popover>
{/if}
