<script lang="ts">
  import Flow from "$lib/components/utils/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
  import { authFlowAuthorize } from "$lib/flows/authorize";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig, canisterId } from "$lib/globals";
  import MarqueeBanner from "$lib/components/ui/MarqueeBanner.svelte";

  analytics.event("page-authorize");

  const connection = new Connection(canisterId.toText(), canisterConfig);
</script>

<MarqueeBanner
  content="The subnet hosting Internet Identity will undergo scheduled maintenance on Thursday, June 26, starting at 8:00 AM UTC, with an expected duration of 5-10 minutes. During this time, new logins may be temporarily unavailable. Existing sessions will remain active. Click here to learn more!"
  speed={20}
  pauseOnHover={true}
  href="https://forum.dfinity.org/t/vetkeys-production-key-and-ii-subnet-downtime/50370"
  fixed={true}
/>
<Flow promise={authFlowAuthorize} args={[connection]} />
