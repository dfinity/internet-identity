<script lang="ts">
  import Flow from "$lib/components/utils/Flow.svelte";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { callbackFlow } from "$lib/legacy/flows/redirect";

  analytics.event("page-redirect-callback");
</script>

<Flow promise={callbackFlow} args={[]} />
