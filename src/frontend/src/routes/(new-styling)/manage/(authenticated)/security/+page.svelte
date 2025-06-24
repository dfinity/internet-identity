<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { AppleIcon, Plus, TriangleAlertIcon, Unlink } from "@lucide/svelte";
  import ThreeColListItem from "$lib/components/ui/ThreeColListItem.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";

  let displayUnlinkGoogleDialog = $state(false);

  const handleAddCredential = () => {
    console.log("add credential"); //TODO
  };
</script>

<div class="text-text-primary">
  <h1 class="mb-4 text-4xl">Security</h1>
  <h2 class="mb-12 text-lg">
    Settings and recommendations to keep your identity secure
  </h2>
  <Panel>
    <div class="flex flex-col justify-between gap-5 p-4 md:flex-row">
      <div>
        <h3 class="mb-2 text-lg font-semibold">Access methods</h3>
        <h4>Manage your passkeys, security keys, and linked accounts.</h4>
      </div>

      <div>
        <Button
          onclick={handleAddCredential}
          class="bg-bg-brand-solid text-text-primary-inversed text-[] top-0 flex w-full items-center justify-center gap-1 rounded-sm px-3.5 py-2 font-semibold md:max-w-fit"
          >Add <Plus size="1.25rem" /></Button
        >
      </div>
    </div>
    <ul>
      {#each identityInfo.devices as device}
        <ThreeColListItem>
          {#snippet colOne()}
            <div class="flex items-center gap-3">
              <AppleIcon />
              <AccessMethod accessMethod={device} />
            </div>
          {/snippet}
          {#snippet colTwo()}
            <h5 class="text-text-tertiary text-sm">Goo goo gaga</h5>
            <AccessMethod accessMethod={device} />
          {/snippet}
          {#snippet colThree()}
            <!-- for layout consistency -->
            <div class="w-[52px]"></div>
          {/snippet}
        </ThreeColListItem>
      {/each}
      <ThreeColListItem>
        {#snippet colOne()}
          <div class="flex items-center gap-3">
            <AppleIcon />
            <h5 class="text-text-primary text-sm font-semibold">Label</h5>
          </div>
        {/snippet}
        {#snippet colTwo()}
          <h5 class="text-text-tertiary text-sm">Goo goo gaga</h5>
        {/snippet}
        {#snippet colThree()}
          <!-- for layout consistency -->
          <div class="w-[52px]"></div>
        {/snippet}
      </ThreeColListItem>
      <ThreeColListItem>
        {#snippet colOne()}
          <div class="flex items-center gap-3">
            <GoogleIcon />
            <h5 class="text-text-primary text-sm font-semibold">
              Account Name
            </h5>
          </div>
        {/snippet}
        {#snippet colTwo()}
          <h5 class="text-text-tertiary text-sm">My gmail acc</h5>
        {/snippet}
        {#snippet colThree()}
          <Button
            variant="tertiary"
            onclick={() => (displayUnlinkGoogleDialog = true)}
          >
            <Unlink class="stroke-fg-error-secondary" />
          </Button>
        {/snippet}
      </ThreeColListItem>
    </ul>
  </Panel>
</div>

{#if displayUnlinkGoogleDialog}
  <Dialog onClose={() => (displayUnlinkGoogleDialog = false)}>
    <FeaturedIcon class="mb-3" variant="warning"
      ><TriangleAlertIcon /></FeaturedIcon
    >
    <h1 class="text-text-primary mb-3 text-2xl font-medium">Are you sure?</h1>
    <p class="text-text-tertiary mb-8 font-medium">
      You're about to unlink your Google Account. If you proceed, you will no
      longer be able to sign-in to your identity or dapps using your Google
      Account.
    </p>
    <div class="flex w-full flex-col gap-3">
      <Button variant="primary" danger>Unlink Google Account</Button>
      <Button variant="tertiary">Keep linked</Button>
    </div>
  </Dialog>
{/if}
