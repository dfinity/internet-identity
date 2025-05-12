<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Props = Omit<HTMLAttributes<HTMLSpanElement>, "children"> & {
    text: string;
    overflow?: boolean;
    position?: "end" | "middle" | "start";
  };

  let {
    class: classname,
    text,
    overflow = $bindable(false),
    position = "end",
    ...props
  }: Props = $props();

  let availableWidth = $state(0);
  let labelWidth = $state(0);

  const leftCharacters = $derived(
    text.replace(/ /g, "\u00a0").match(/\s*./g) as string[],
  );
  const rightCharacters = $derived(
    text.replace(/ /g, "\u00a0").match(/.\s*/g)!.reverse() as string[],
  );

  $effect(() => {
    overflow = labelWidth > availableWidth;
  });
</script>

<span
  {...props}
  class={["relative flex items-start overflow-hidden break-all", classname]}
>
  <span
    bind:clientWidth={availableWidth}
    class={[
      "overflow-hidden text-clip whitespace-nowrap",
      overflow && "opacity-0",
    ]}
  >
    {text}
  </span>
  <span
    bind:clientWidth={labelWidth}
    class="pointer-events-none absolute whitespace-nowrap opacity-0"
    aria-hidden="true"
  >
    {text}
  </span>
  <span
    class={["pointer-events-none absolute inset-0 flex", !overflow && "hidden"]}
    aria-hidden="true"
  >
    {#if position !== "start"}
      <span class="flex flex-1 flex-wrap justify-end">
        {#each leftCharacters as character}
          <span>{character}</span>
        {/each}
      </span>
    {/if}
    <span>…</span>
    {#if position !== "end"}
      <span class="flex flex-1 flex-row-reverse flex-wrap justify-end">
        {#each rightCharacters as character}
          <span>{character}</span>
        {/each}
      </span>
    {/if}
  </span>
</span>
