<script lang="ts">
  //This is the screen where you authenticate with a dapp
  //Signing in here gets you to a dapp
  //I called it authorize to mirror the existing nomenclature

  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import Button from "$lib/components/UI/Button.svelte";
  import PasskeyCard from "$lib/components/UI/PasskeyCard.svelte";
  import CenterCard from "$lib/components/UI/CenterCard.svelte";
  import { goto } from "$app/navigation";
  import { isNullish } from "@dfinity/utils";

  let showPasskeyCard = $state(false);

  // TODO: get dapp name via postmessageinterface
  let dappName = $state<string>("DAPP");
  // TODO: this should really be pulled from a central store that initializes itself on load
  // TODO: of course we would also need to pull account/profile/role/login info, but one thing
  // TODO: after another
  let lastUsedIdentity = $state<string | undefined>("gargle");

  const handleContinueWithPasskey = () => {
    showPasskeyCard = true;
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
    goto("/new-authorize/name-identity");
  };

  const handleContinueWithLastUsedIdentity = () => {
    //TODO
    console.log("continuing with last used");
  };

  const handleContinueWithOtherIdentity = () => {
    lastUsedIdentity = undefined;
  };
</script>

<!-- an element with id 'newAuthenticateTitle' is necessary for the e2e tests to pass -->
<CenterContainer data-role="new-authorize-view">
  <CenterCard>
    <div class="flex flex-col gap-1">
      <h1 class="h1 font-bold">[Sign in]</h1>
      <p class="p font-medium">
        to continue with <span class="font-bold">{dappName}</span>
      </p>
    </div>
    {#if isNullish(lastUsedIdentity)}
      <Button
        onclick={handleContinueWithPasskey}
        class="w-full"
        variant="primary">Continue with Passkey</Button
      >
      <Button
        onclick={handleContinueWithGoogle}
        class="w-full"
        variant="secondary">Continue with Google</Button
      >
    {:else}
      <!-- TODO: here we would actually select the account, not the identity -->
      <!-- TODO: text-left not working -->
      <Button
        onclick={handleContinueWithLastUsedIdentity}
        class="w-full px-6 py-4 text-left"
        variant="primary">Continue as {lastUsedIdentity}</Button
      >
      <Button
        onclick={handleContinueWithOtherIdentity}
        class="w-full px-6 py-4 text-left"
        variant="dashed">Use another Internet Identity</Button
      >
    {/if}
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
