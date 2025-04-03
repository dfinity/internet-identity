<script lang="ts">
  import Flow from "$lib/components/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
  import { authFlowAuthorize } from "$lib/flows/authorize";
  import { analytics } from "$lib/utils/analytics/analytics";

  analytics.event("page-authorize");

  const connection = new Connection(readCanisterId(), readCanisterConfig());
</script>

<Flow promise={authFlowAuthorize} args={[connection]} />
