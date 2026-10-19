<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import Logo from "$lib/components/ui/Logo.svelte";

  const ghostTile =
    "size-21 rounded-[20px] bg-fg-primary/5 ring ring-fg-primary/5 ring-inset";
  const ghostName = "my-1 h-2 w-[46px] rounded-sm bg-fg-primary/5";
  /** What one cell comes to, so the grid in front keeps the hole the real tile sits in. */
  const cellHeight = "h-29";
</script>

<div
  aria-hidden="true"
  class="relative grid grid-cols-[repeat(3,84px)] gap-x-[30px] gap-y-6"
>
  <!-- The apps around it are furniture, faded out towards the edges so the one that was
       just added reads as one app among many rather than a grid of nine. -->
  <div
    class="absolute -inset-5 grid grid-cols-[repeat(3,84px)] gap-x-[30px] gap-y-6 [mask-image:radial-gradient(circle_at_50%_50%,#000_30%,transparent_72%)] p-5"
  >
    {#each [0, 1, 2, 3] as cell (cell)}
      <span class="flex flex-col items-center gap-2">
        <span class={ghostTile}></span>
        <span class={ghostName}></span>
      </span>
    {/each}
    <!-- The gap the real tile sits in. -->
    <span></span>
    {#each [4, 5, 6, 7] as cell (cell)}
      <span class="flex flex-col items-center gap-2">
        <span class={ghostTile}></span>
        <span class={ghostName}></span>
      </span>
    {/each}
  </div>

  {#each [0, 1, 2, 3] as cell (cell)}
    <span class={cellHeight}></span>
  {/each}
  <span class="relative flex flex-col items-center gap-2">
    <span
      class="flex size-21 shrink-0 items-center justify-center rounded-[20px] bg-linear-to-b from-[#2a2a2d] to-[#1a1a1c] text-white shadow-[inset_0_0_0_1px_rgba(255,255,255,.14),inset_0_1px_0_rgba(255,255,255,.12),0_24px_48px_rgba(0,0,0,.5)]"
    >
      <Logo class="block h-[22px] w-[46px]" />
    </span>
    <span class="text-text-primary text-[13px]/4">{$t`ICP Alerts`}</span>
  </span>
  {#each [4, 5, 6, 7] as cell (cell)}
    <span class={cellHeight}></span>
  {/each}
</div>
