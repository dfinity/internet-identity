<script lang="ts">
  import { addDevice } from "$src/flows/addDevice/manage/addDevice";
  import { getConnectionContext } from "../../context.svelte";
  import Flow from "../../../../lib/Flow.svelte";
  import { goto } from "$app/navigation";
  import Template from "../../../../lib/Template.svelte";
  import { promptUserNumberTemplate } from "$src/components/promptUserNumber";

  const connection = getConnectionContext();
</script>

<Flow
  promise={addDevice}
  args={[{
    userNumber: connection.userNumber,
    connection,
    origin: window.location.origin
  }]}
  onResolve={() => goto('/')}
/>
<!-- Example of rendering Lit within Svelte -->
<Template
  render={promptUserNumberTemplate}
  args={[{
    title: 'Number pls?',
    onContinue: (number) => {
      alert(number)
    },
    onCancel: () => {},
  }]}
/>
