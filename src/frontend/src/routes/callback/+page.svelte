<script lang="ts">
  import Flow from "$lib/components/Flow.svelte";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { callbackFlow } from "$lib/flows/redirect";

  analytics.event("page-redirect-callback");
</script>

<Flow promise={callbackFlow} args={[]} />
