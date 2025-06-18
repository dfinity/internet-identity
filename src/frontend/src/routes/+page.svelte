<script lang="ts">
  import Flow from "$lib/components/utils/Flow.svelte";
  import { Connection } from "$lib/utils/iiConnection";
  import { authFlowManage } from "$lib/legacy/flows/manage";
  import { analytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig, canisterId } from "$lib/globals";
  import MarqueeBanner from "$lib/components/ui/MarqueeBanner.svelte";
  import TimedDisplay from "$lib/components/utils/TimedDisplay.svelte";

  analytics.event("page-manage");

  const connection = new Connection(canisterId.toText(), canisterConfig);
</script>

<TimedDisplay
  appearTime={new Date(Date.UTC(2025, 5, 23, 0))}
  disappearTime={new Date(Date.UTC(2025, 5, 26, 8, 5))}
>
  <MarqueeBanner
    content="The subnet hosting Internet Identity will undergo scheduled maintenance on Thursday, June 26, starting at 8:00 AM UTC, with an expected duration of 5-10 minutes. During this time, log in will be temporarily unavailable. Existing sessions will remain active. Click here to learn more"
    speed={20}
    pauseOnHover={true}
    href="https://forum.dfinity.org/t/vetkeys-production-key-and-ii-subnet-downtime/50370"
    fixed={true}
  />
</TimedDisplay>
<Flow promise={authFlowManage} args={[connection]} />
