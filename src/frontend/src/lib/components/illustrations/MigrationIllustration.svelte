<script lang="ts">
  import type { SvelteHTMLElements, SVGAttributes } from "svelte/elements";
  import LogoLegacy from "./LogoLegacy.svelte";
  import { ArrowRightIcon } from "@lucide/svelte";
  import Logo from "../ui/Logo.svelte";

  type Props = SvelteHTMLElements["div"];

  const { class: className, ...props }: Props = $props();
</script>

<div
  {...props}
  class={["flex items-center justify-center gap-4 py-3.5", className]}
>
  <LogoLegacy class="h-8" />
  <ArrowRightIcon class="size-6" />
  <Logo class="h-8" />
</div>
