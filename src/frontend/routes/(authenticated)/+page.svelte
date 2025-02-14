<script lang="ts">
  import { renderManage } from "$src/flows/manage";
  import Flow from "../../lib/Flow.svelte";
  import { PreLoadImage } from "$src/utils/preLoadImage";
  import identityCardBackground from "$src/assets/identityCardBackground.png";
  import { getConnectionContext } from "./context.svelte";

  const connection = getConnectionContext();
  const identityBackground = new PreLoadImage(identityCardBackground);
</script>

<Flow
  promise={renderManage}
  args={[{
    userNumber: connection.userNumber,
    connection,
    identityBackground
  }]}
/>
