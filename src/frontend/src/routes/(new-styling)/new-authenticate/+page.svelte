<script lang="ts">
  // This is the vanilla landing screen
  // Signing in here gets you to the dashboard

  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import Button from "$lib/components/UI/Button.svelte";
  import PasskeyCard from "$lib/components/UI/PasskeyCard.svelte";
  import CenterCard from "$lib/components/UI/CenterCard.svelte";
  import { isNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/UI/Dialog.svelte";
  import NameIdentityCard from "$lib/components/UI/NameIdentityCard.svelte";

  // TODO: this should really be pulled from a central store that initializes itself on load
  // TODO: of course we would also need to pull account/profile/role/login info, but one thing
  // TODO: after another
  let lastUsedIdentity = $state<string | undefined>();
  let showPasskeyCard = $state(false);
  let creatingIdentity = $state(false);

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

  const handleGotoCreateIdentity = () => {
    creatingIdentity = true;
  };

  const handleContinueWithLastUsedIdentity = () => {
    //TODO
    console.log("continuing with last used");
  };

  const handleContinueWithOtherIdentity = () => {
    lastUsedIdentity = undefined;
  };

  const handleCreateIdentity = () => {
    // TODO
  };

  const close = () => {
    showPasskeyCard = false;
    creatingIdentity = false;
    hasTransitionedOut = false;
  };

  //TODO: there are be more elegant ways to do this
  let hasTransitionedOut = $state(false);

  const transitionedOut = () => {
    hasTransitionedOut = true;
  };
</script>

<CenterContainer data-role="new-authenticate-view">
  <CenterCard>
    <div class="flex flex-col gap-1">
      <h1 class="h1 font-bold">[Sign in]</h1>
      <p class="p font-medium">to your Internet Identity</p>
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
    <Dialog onClose={close}>
      {#if !creatingIdentity}
        <FlyWrapper handleTransitionEnd={transitionedOut}>
          <PasskeyCard
            {handleConnectPasskey}
            {handleGotoCreateIdentity}
            {close}
          />
        </FlyWrapper>
      {:else if hasTransitionedOut}
        <!-- TODO: handle resizing -->
        <FlyWrapper delay={300}>
          <NameIdentityCard class="relative" {handleCreateIdentity} {close} />
        </FlyWrapper>
      {/if}
    </Dialog>
  {/if}
</CenterContainer>
