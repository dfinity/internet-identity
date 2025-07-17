<script lang="ts">
  import Flow from "$lib/components/utils/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { webAuthnInIframeFlow } from "$lib/legacy/flows/iframeWebAuthn";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig, canisterId } from "$lib/globals";

  analytics.event("page-webauthn-iframe");

  const connection = new Connection(canisterId.toText(), canisterConfig);
</script>

<Flow promise={webAuthnInIframeFlow} args={[connection]} />
