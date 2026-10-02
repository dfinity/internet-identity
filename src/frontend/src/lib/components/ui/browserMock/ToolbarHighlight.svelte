<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  type Props = SvelteHTMLElements["div"] & {
    /** The radii the rings are drawn with. `pill` rings a fully rounded control;
     *  `menu` rings a 5px-radius one, each ring widening by its own inset. */
    shape?: "pill" | "menu";
  };

  const {
    children,
    class: className,
    shape = "pill",
    ...props
  }: Props = $props();

  const rings =
    shape === "pill"
      ? ["rounded-full", "rounded-full", "rounded-full", "rounded-full"]
      : [
          "rounded-[10px]",
          "rounded-[15px]",
          "rounded-[19px]",
          "rounded-[23px]",
        ];
</script>

<div {...props} class={["relative", className]}>
  {@render children?.()}
  <div
    class={[
      "absolute -inset-[5px] border-[1px] border-blue-700 dark:border-blue-300",
      rings[0],
    ]}
  ></div>
  <div
    class={[
      "absolute -inset-[10px] border-[0.75px] border-blue-500 dark:border-blue-500",
      rings[1],
    ]}
  ></div>
  <div
    class={[
      "absolute -inset-[14px] border-[0.5px] border-blue-300 dark:border-blue-700",
      rings[2],
    ]}
  ></div>
  <div
    class={[
      "absolute -inset-[18px] border-[0.25px] border-blue-200 dark:border-blue-800",
      rings[3],
    ]}
  ></div>
</div>
