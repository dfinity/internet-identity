<script lang="ts">
  import Flow from "$lib/components/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
  import { authFlowManage } from "$lib/flows/manage";
  import { analytics } from "$lib/utils/analytics/analytics";

  analytics.event("page-manage");

  const connection = new Connection(readCanisterId(), readCanisterConfig());
</script>

<Flow promise={authFlowManage} args={[connection]} />
