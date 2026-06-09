<script lang="ts">
  import { CheckIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import type { HTMLButtonAttributes } from "svelte/elements";
  import { HoldController } from "./holdController";

  interface Props extends Omit<
    HTMLButtonAttributes,
    "class" | "disabled" | "type" | "aria-label"
  > {
    duration?: number;
    label: string;
    completed?: boolean;
    onComplete?: () => void;
    class?: string;
  }

  let {
    duration = 2500,
    label,
    completed = false,
    onComplete,
    class: className,
    ...props
  }: Props = $props();

  let progress = $state(0);

  const controller = new HoldController({
    getDuration: () => duration,
    onProgress: (v) => (progress = v),
    onComplete: () => onComplete?.(),
  });

  $effect(() => {
    if (completed) {
      progress = 1;
    } else {
      controller.reset();
    }
  });

  $effect(() => () => controller.dispose());
</script>

<button
  {...props}
  type="button"
  disabled={completed}
  aria-label={completed ? $t`Confirmed` : label}
  onmousedown={() => {
    if (!completed) controller.start();
  }}
  onmouseup={() => controller.cancel()}
  onmouseleave={() => controller.cancel()}
  ontouchstart={(e) => {
    if (completed) return;
    e.preventDefault();
    controller.start();
  }}
  ontouchend={() => controller.cancel()}
  ontouchcancel={() => controller.cancel()}
  onkeydown={(e) => {
    if (e.code !== "Space" || e.repeat || completed) return;
    e.preventDefault();
    controller.start();
  }}
  onkeyup={(e) => {
    if (e.code !== "Space") return;
    e.preventDefault();
    controller.cancel();
  }}
  class={[
    "bg-bg-primary focus-visible:ring-offset-bg-primary focus-visible:ring-focus-ring relative box-border flex h-12 w-full touch-none items-center justify-center overflow-hidden rounded-md border px-4.5 text-base font-semibold outline-none select-none not-disabled:cursor-pointer focus-visible:ring-2 focus-visible:ring-offset-2",
    completed
      ? "border-border-brand text-text-primary-inversed"
      : "border-border-secondary text-text-primary",
    "transition-colors duration-200",
    className,
  ]}
>
  <span
    aria-hidden="true"
    class={[
      "absolute inset-0 origin-left",
      completed ? "bg-bg-brand-solid" : "bg-bg-primary_hover",
    ]}
    style="width: {progress * 100}%"
  ></span>
  <span class="relative inline-flex items-center gap-2.5">
    {#if completed}
      <CheckIcon class="size-5" />
      <span>{$t`Confirmed`}</span>
    {:else}
      <span>{label}</span>
    {/if}
  </span>
</button>

<style>
  button,
  button * {
    user-select: none;
    -webkit-user-select: none;
    -webkit-touch-callout: none;
  }
</style>
