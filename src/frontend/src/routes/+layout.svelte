<script lang="ts">
  import { readCanisterConfig } from "$lib/utils/init";
  import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";
  // TODO: Enable new styles only in the new layout pages.
  // import "./app.css";

  const { children } = $props();

  const canisterConfig = readCanisterConfig();
  initAnalytics(canisterConfig.analytics_config[0]?.[0]);
  analytics.pageView();
</script>

{@render children()}
