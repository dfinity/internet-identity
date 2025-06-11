<script lang="ts">
  import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";
  import MarqueeBanner from "$lib/components/ui/MarqueeBanner.svelte";
  import { canisterConfig } from "$lib/globals";

  const { children } = $props();

  initAnalytics(canisterConfig.analytics_config[0]?.[0]);
  analytics.pageView();
</script>

<MarqueeBanner
  content="The subnet hosting Internet Identity will undergo scheduled maintenance on Thursday, June 26, starting at 8:00 AM UTC, with an expected duration of 5-10 minutes. During this time, new logins may be temporarily unavailable. Existing sessions will remain active. Click here to learn more!"
  speed={20}
  pauseOnHover={true}
  href="https://forum.dfinity.org/"
  fixed={true}
/>
{@render children()}
