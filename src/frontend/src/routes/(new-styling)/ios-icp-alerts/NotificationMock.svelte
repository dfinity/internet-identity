<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import Logo from "$lib/components/ui/Logo.svelte";

  const { title, body }: { title: string; body: string } = $props();

  const ghostRow =
    "flex h-[62px] items-center gap-3 rounded-3xl bg-fg-primary/6 pr-3.5 pl-3 ring ring-fg-primary/6 ring-inset";
  const ghostTile = "size-9 rounded-[9px] bg-fg-primary/6";
  const ghostBar = "h-2 rounded-sm bg-fg-primary/6";
</script>

<div
  aria-hidden="true"
  class="relative flex h-[396px] w-[330px] items-center justify-center"
>
  <!-- The rows behind are furniture, faded out towards the edges so the card in
       front reads as one notification among many rather than a list of five. -->
  <div
    class="absolute -inset-5 [mask-image:radial-gradient(circle_at_50%_50%,#000_30%,transparent_72%)] p-5"
  >
    <div class="flex h-full flex-col justify-center gap-3">
      {#each [0, 1] as row (row)}
        <div class={ghostRow}>
          <span class={ghostTile}></span>
          <span class="flex flex-col gap-2">
            <span class="{ghostBar} w-30"></span>
            <span class="{ghostBar} w-45"></span>
          </span>
        </div>
      {/each}
      <!-- The gap the real card sits in. -->
      <div class="h-[62px]"></div>
      {#each [2, 3] as row (row)}
        <div class={ghostRow}>
          <span class={ghostTile}></span>
          <span class="flex flex-col gap-2">
            <span class="{ghostBar} w-30"></span>
            <span class="{ghostBar} w-45"></span>
          </span>
        </div>
      {/each}
    </div>
  </div>

  <div
    class="bg-bg-secondary ring-border-secondary relative flex w-[330px] items-center gap-3 rounded-3xl py-[11px] pr-3.5 pl-3 text-left font-sans tracking-[-0.015em] shadow-[0_20px_40px_rgba(0,0,0,.18)] ring ring-inset"
  >
    <span
      class="flex size-9 shrink-0 items-center justify-center rounded-[9px] bg-linear-to-b from-[#2a2a2d] to-[#1a1a1c] text-white shadow-[inset_0_0_0_1px_rgba(255,255,255,.14),inset_0_1px_0_rgba(255,255,255,.12)]"
    >
      <Logo class="block h-[10.5px] w-[22px]" />
    </span>
    <div class="flex min-w-0 flex-1 flex-col">
      <div class="flex items-baseline justify-between gap-2">
        <span class="text-text-primary truncate text-sm/[19px] font-semibold">
          {title}
        </span>
        <span class="text-text-tertiary shrink-0 text-xs/[19px]">{$t`now`}</span
        >
      </div>
      <span class="text-text-secondary truncate text-sm/[19px]">{body}</span>
    </div>
  </div>
</div>
