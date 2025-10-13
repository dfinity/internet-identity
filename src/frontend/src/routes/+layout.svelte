<script lang="ts">
  import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig } from "$lib/globals";
  import { onMount } from "svelte";

  const { children } = $props();

  onMount(() => {
    // Show page once it has fully loaded (see app.html)
    document.documentElement.removeAttribute("data-temp-hide-ssg");

    initAnalytics(canisterConfig.analytics_config[0]?.[0]);
    analytics.pageView();
  });
</script>

{@render children()}
