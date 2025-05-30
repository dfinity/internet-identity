<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = Omit<SVGAttributes<SVGSVGElement>, "seed"> & {
    seed: bigint;
    elements?: number;
  };

  const { class: className, seed, elements = 4, ...props }: Props = $props();

  const colors = [
    "#29abe2",
    "#f15a24",
    "#fbb03b",
    "#36b26a",
    "#6923bd",
    "#ed1e79",
  ];
</script>

<svg
  {...props}
  viewBox="0 0 {elements} {elements}"
  class={["rounded-full", className]}
>
  {#each { length: elements } as _, x}
    {#each { length: elements } as _, y}
      <rect
        {x}
        {y}
        width={1.1}
        height={1.1}
        fill={colors[
          Number(
            ((BigInt(1000) + seed) % BigInt(x * elements + y + 1)) %
              BigInt(colors.length - 1),
          )
        ]}
      />
    {/each}
  {/each}
</svg>
