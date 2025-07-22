<script lang="ts">
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import type { HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLDivElement> & {
    origin: string;
  };

  const { class: className, origin, ...props }: Props = $props();
</script>

<div {...props} class={["flex flex-1 flex-col", className]}>
  <div class="flex flex-1 flex-col items-center justify-center pt-5 pb-6">
    <div
      aria-hidden="true"
      class={[
        "mb-6 flex shrink-0 items-center justify-center overflow-hidden rounded-2xl",
      ]}
    >
      <div class="text-6xl">🦜</div>
    </div>
    <Badge size="sm" class="max-w-[75%]">
      <Ellipsis
        text={"uqsxe-waaaa-aaaao-qkela-cai.icp0.io"}
        position="middle"
      />
    </Badge>
  </div>
</div>
