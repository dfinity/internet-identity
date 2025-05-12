<script lang="ts">
  import Flow from "$lib/components/utils/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { authFlowManage } from "$lib/flows/manage";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig, canisterId } from "$lib/globals";

  analytics.event("page-manage");

  const connection = new Connection(canisterId.toText(), canisterConfig);
</script>

<Flow promise={authFlowManage} args={[connection]} />
