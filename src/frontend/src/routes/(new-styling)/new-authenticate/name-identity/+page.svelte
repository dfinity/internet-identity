<script lang="ts">
  import { replaceState } from "$app/navigation";
  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import NameIdentityCard from "$lib/components/UI/NameIdentityCard.svelte";
  import { onMount } from "svelte";

  const handleCreatePasskey = (name: string) => {
    //TODO
    console.log("creating passkey with name " + name);
  };

  onMount(() => {
    replaceState("", {}); //TODO: not yet working
  });
</script>

<CenterContainer>
  <NameIdentityCard {handleCreatePasskey} />
</CenterContainer>
