<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import Logo from "$lib/components/ui/Logo.svelte";

  const {
    /** Draws the tap rings on the tile, for a grid that is asking to be tapped rather
     *  than reporting a finished install. */
    tap = false,
  }: { tap?: boolean } = $props();

  const ghostTile =
    "size-21 rounded-[20px] bg-fg-primary/5 ring ring-fg-primary/5 ring-inset";
  const ghostName = "my-1 h-2 w-[46px] rounded-sm bg-fg-primary/5";
  /** What one cell comes to, so the grid in front keeps the hole the real tile sits in. */
  const cellHeight = "h-29";
  /** The rings reach further than the furniture does, so it is pulled in behind them. */
  const fade = $derived(
    tap
      ? "[mask-image:radial-gradient(circle_at_50%_50%,#000_26%,transparent_66%)]"
      : "[mask-image:radial-gradient(circle_at_50%_50%,#000_30%,transparent_72%)]",
  );
  /** The install steps' tap rings, spread for a target this size and following the
   *  tile's own 20px corner rather than the toolbar radii those presets carry. Every
   *  ring is a whole pixel wide and fades through its colour: a border thinner than one
   *  device pixel survives on one edge and rounds away on the other. */
  const rings = [
    "-inset-2 rounded-[28px] border-blue-300",
    "-inset-[17px] rounded-[37px] border-blue-500/80",
    "-inset-[26px] rounded-[46px] border-blue-700/60",
    "-inset-[35px] rounded-[55px] border-blue-800/40",
  ];
</script>

<div
  aria-hidden="true"
  class="relative grid grid-cols-[repeat(3,84px)] gap-x-[30px] gap-y-6"
>
  <!-- The apps around it are furniture, faded out towards the edges so the one that was
       just added reads as one app among many rather than a grid of nine. -->
  <div
    class={[
      "absolute -inset-5 grid grid-cols-[repeat(3,84px)] gap-x-[30px] gap-y-6 p-5",
      fade,
    ]}
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
      class="relative flex size-21 shrink-0 items-center justify-center rounded-[20px] bg-linear-to-b from-[#2a2a2d] to-[#1a1a1c] text-white shadow-[inset_0_0_0_1px_rgba(255,255,255,.14),inset_0_1px_0_rgba(255,255,255,.12),0_24px_48px_rgba(0,0,0,.5)]"
    >
      <Logo class="block h-[22px] w-[46px]" />
      {#if tap}
        {#each rings as ring (ring)}
          <span class={["absolute border", ring]}></span>
        {/each}
      {/if}
    </span>
    <span class="text-text-primary text-[13px]/4">{$t`ICP Alerts`}</span>
  </span>
  {#each [4, 5, 6, 7] as cell (cell)}
    <span class={cellHeight}></span>
  {/each}
</div>
