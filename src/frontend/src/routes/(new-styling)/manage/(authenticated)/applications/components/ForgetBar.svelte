<script lang="ts">
  import { Trash2Icon } from "@lucide/svelte";
  import { plural, t } from "$lib/stores/locale.store";

  interface Props {
    selecting: boolean;
    count: number;
    onStart: () => void;
    onCancel: () => void;
    onForget: () => void;
  }

  const { selecting, count, onStart, onCancel, onForget }: Props = $props();

  // The pill is as wide as its label in the user's language, and a width can only be
  // animated between lengths, so the toolbar grows from the measured one.
  let pillWidth = $state<number>();
  const width = $derived(
    selecting
      ? "100%"
      : pillWidth !== undefined
        ? `calc(${pillWidth}px + 2px)`
        : undefined,
  );
</script>

<!-- The scrim behind the bar while it sticks. Its padding and margins cancel out, so
     the toolbar can grow over the space around it without moving the list. -->
<div
  class={[
    "from-bg-primary_alt via-bg-primary_alt sticky z-20 -mx-4 -mt-11 -mb-3 box-content flex h-9 items-end bg-linear-to-b from-0% via-76% to-transparent px-4 pt-11 pb-9 ease-[cubic-bezier(.4,0,.2,1)] motion-safe:transition-[top] motion-safe:duration-300",
    selecting ? "top-0" : "-top-5",
  ]}
>
  <div
    role="toolbar"
    aria-label={$t`Remove apps`}
    style:width
    class={[
      "border-border-secondary bg-bg-primary relative flex shrink-0 items-center overflow-hidden border ease-[cubic-bezier(.4,0,.2,1)] motion-safe:transition-[width,height,margin-bottom,border-radius,box-shadow] motion-safe:duration-300",
      selecting ? "-mb-2 h-16 rounded-xl shadow-lg" : "h-9 rounded-[18px]",
    ]}
  >
    <button
      bind:offsetWidth={pillWidth}
      onclick={onStart}
      inert={selecting}
      class={[
        "text-fg-primary hover:bg-bg-primary_hover inline-flex h-full shrink-0 cursor-pointer items-center justify-center gap-1.5 rounded-full px-3 text-sm font-semibold whitespace-nowrap outline-none motion-safe:transition-opacity motion-safe:duration-150",
        selecting ? "opacity-0" : "opacity-100 motion-safe:delay-150",
      ]}
    >
      <Trash2Icon class="size-3.5 shrink-0" aria-hidden="true" />
      {$t`Remove apps`}
    </button>
    <div
      inert={!selecting}
      class={[
        "absolute inset-0 flex items-center gap-2 ps-4 pe-3 motion-safe:transition-opacity motion-safe:duration-200",
        selecting ? "opacity-100 motion-safe:delay-150" : "opacity-0",
      ]}
    >
      <span aria-live="polite" class="flex min-w-0 flex-1 flex-col text-sm">
        <span class="text-text-primary truncate font-semibold">
          {count === 0
            ? $t`Select apps to remove`
            : $plural(count, {
                one: "1 app selected",
                other: "# apps selected",
              })}
        </span>
        <span class="text-text-tertiary truncate">
          {count === 0
            ? $t`You'll be signed out and permissions revoked.`
            : count === 1
              ? $t`You'll be signed out and its permissions revoked.`
              : $t`You'll be signed out and their permissions revoked.`}
        </span>
      </span>
      <button onclick={onCancel} class="btn btn-tertiary btn-md">
        {$t`Cancel`}
      </button>
      <button
        onclick={onForget}
        disabled={count === 0}
        class="btn btn-primary btn-md btn-danger"
      >
        {$t`Remove`}
      </button>
    </div>
  </div>
</div>
