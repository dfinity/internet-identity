<script lang="ts">
  import Flow from "$lib/components/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { vcFlow } from "$lib/flows/verifiableCredentials";

  analytics.event("page-webauthn-iframe");

  const connection = new Connection(readCanisterId(), readCanisterConfig());
</script>

<Flow promise={vcFlow} args={[{ connection }]} />
