<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";

  type Props = HTMLAttributes<HTMLDivElement>;

  const { children, class: className, ...rest }: Props = $props();
</script>

<div class={["flex min-h-[100dvh] flex-col", className]} {...rest}>
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    {@render children?.()}
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
