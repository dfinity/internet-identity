<script lang="ts">
  import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig } from "$lib/globals";

  const { children } = $props();

  initAnalytics(canisterConfig.analytics_config[0]?.[0]);
  analytics.pageView();
</script>

{@render children()}
