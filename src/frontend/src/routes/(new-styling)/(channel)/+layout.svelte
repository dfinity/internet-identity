<script lang="ts">
  import type { LayoutProps } from "./$types";
  import AuthorizationChannel from "$lib/components/utils/AuthorizationChannel.svelte";

  const { children }: LayoutProps = $props();
</script>

<AuthorizationChannel>
  {@render children()}
</AuthorizationChannel>
