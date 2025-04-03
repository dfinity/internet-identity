<script lang="ts">
  import Flow from "$lib/components/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
  import { webAuthnInIframeFlow } from "$lib/flows/iframeWebAuthn";
  import { analytics } from "$lib/utils/analytics/analytics";

  analytics.event("page-webauthn-iframe");

  const connection = new Connection(readCanisterId(), readCanisterConfig());
</script>

<Flow promise={webAuthnInIframeFlow} args={[connection]} />
