<script lang="ts">
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import type { LayoutProps } from "../$types";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";

  const { children }: LayoutProps = $props();
</script>

<div class="flex min-h-[100dvh] flex-col" data-page="new-authorize-view">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <div
      class="flex flex-1 flex-row items-end justify-center sm:max-w-120 sm:items-center"
    >
      <AuthPanel>
        {@render children()}
      </AuthPanel>
    </div>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
