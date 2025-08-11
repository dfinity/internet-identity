<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";
  import LogoLegacy from "./LogoLegacy.svelte";
  import { ArrowRightIcon } from "@lucide/svelte";
  import Logo from "../ui/Logo.svelte";

  type Props = SVGAttributes<SVGSVGElement>;

  const { class: className, ...props }: Props = $props();
</script>

<div class="flex items-center justify-center gap-4 py-3.5">
  <LogoLegacy class="h-8" />
  <ArrowRightIcon class="size-6" />
  <Logo class="h-8" />
</div>
