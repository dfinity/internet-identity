<script lang="ts">
  import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig } from "$lib/globals";
  import { onMount } from "svelte";

  const { children } = $props();

  onMount(() => {
    initAnalytics(canisterConfig.analytics_config[0]?.[0]);
    analytics.pageView();
  });
</script>

{@render children()}
