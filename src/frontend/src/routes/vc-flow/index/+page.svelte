<script lang="ts">
  import Flow from "$lib/components/utils/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { vcFlow } from "$lib/legacy/flows/verifiableCredentials";
  import { canisterConfig, canisterId } from "$lib/globals";

  analytics.event("page-webauthn-iframe");

  const connection = new Connection(canisterId.toText(), canisterConfig);
</script>

<Flow promise={vcFlow} args={[{ connection }]} />
