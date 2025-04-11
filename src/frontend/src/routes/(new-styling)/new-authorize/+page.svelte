<script lang="ts">
  //This is the screen where you authenticate with a dapp
  //Signing in here gets you to a dapp
  //I called it authorize to mirror the existing nomenclature

  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import Button from "$lib/components/UI/Button.svelte";
  import PasskeyCard from "$lib/components/UI/PasskeyCard.svelte";
  import CenterCard from "$lib/components/UI/CenterCard.svelte";
  import { goto } from "$app/navigation";

  let showPasskeyCard = $state(true);

  //TODO: get dapp name via postmessageinterface
  let dappName = "DAPP";

  const handleContinueWithPasskey = () => {
    //TODO
    showPasskeyCard = true;
    console.log("continuing with passkey");
  };

  const handleContinueWithGoogle = () => {
    //TODO
    console.log("continuing with google");
  };

  const handleConnectPasskey = () => {
    //TODO
    console.log("connecting passkey");
  };

  const handleGotoCreatePasskey = () => {
    console.log("going to create passkey");
    goto("/new-authorize/name-identity");
  };
</script>

<!-- an element with id 'newAuthenticateTitle' is necessary for the e2e tests to pass -->
<CenterContainer data-role="new-authorize-view">
  <CenterCard>
    <div class="flex flex-col gap-1">
      <h1 class="h1 font-bold">[Sign in]</h1>
      <h2 class="p font-medium">
        to continue with <span class="font-bold">{dappName}</span>
      </h2>
    </div>
    <Button onclick={handleContinueWithPasskey} class="w-full" variant="primary"
      >Continue with Passkey</Button
    >
    <Button
      onclick={handleContinueWithGoogle}
      class="w-full"
      variant="secondary">Continue with Google</Button
    >
  </CenterCard>

  {#if showPasskeyCard}
    <PasskeyCard
      close={() => {
        showPasskeyCard = false;
      }}
      {handleConnectPasskey}
      {handleGotoCreatePasskey}
    />
  {/if}
</CenterContainer>
