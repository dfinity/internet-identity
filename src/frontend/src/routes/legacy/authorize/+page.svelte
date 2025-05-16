<script lang="ts">
  import Flow from "$lib/components/utils/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
  import { authFlowAuthorize } from "$lib/flows/authorize";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig, canisterId } from "$lib/globals";

  analytics.event("page-authorize");

  const connection = new Connection(canisterId.toText(), canisterConfig);
</script>

<Flow promise={authFlowAuthorize} args={[connection]} />
