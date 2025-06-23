<script lang="ts">
  import Alert from "$lib/components/ui/Alert.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Card from "$lib/components/ui/Card.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import { Plus, TriangleAlertIcon, Unlink } from "@lucide/svelte";

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
  <Card>
    <div class="flex flex-col justify-between gap-5 p-4 md:flex-row">
      <div>
        <h3 class="mb-2 text-lg font-semibold">Access methods</h3>
        <h4>Manage your passkeys, security keys and linked accounts.</h4>
      </div>

      <div>
        <Button
          onclick={handleAddCredential}
          class="bg-bg-brand-solid text-text-primary-inversed text-[] top-0 flex w-full items-center justify-center gap-1 rounded-sm px-3.5 py-2 font-semibold md:max-w-fit"
          >Add <Plus size="1.25rem" /></Button
        >
      </div>
    </div>
    <table class="w-full">
      <tbody>
        <tr class="border-border-tertiary border-t border-b">
          <td class="p-4">
            <h5 class="font-semibold">Label</h5>
          </td>
          <td class="p-4">
            <h5>Goo goo gaga</h5>
          </td>
          <td class="flex justify-end p-4">
            <ButtonOrAnchor></ButtonOrAnchor>
          </td>
        </tr>
        <tr>
          <td class="p-4">
            <h5 class="font-semibold">Account Name</h5>
          </td>
          <td class="p-4">
            <h5>My gmail acc</h5>
          </td>
          <td class="flex justify-end p-4"
            ><Button
              variant="tertiary"
              onclick={() => (displayUnlinkGoogleDialog = true)}
            >
              <Unlink class="stroke-fg-error-secondary" />
            </Button></td
          ></tr
        >
      </tbody>
    </table>
  </Card>
</div>

{#if displayUnlinkGoogleDialog}
  <Dialog onClose={() => (displayUnlinkGoogleDialog = false)}>
    <FeaturedIcon class="mb-3" variant="warning"
      ><TriangleAlertIcon /></FeaturedIcon
    >
    <h1 class="text-text-primary mb-3 text-2xl font-medium">Are you sure?</h1>
    <p class="text-text-tertiary mb-8 font-medium">
      You're about to disconnect your Google Account. If you proceed, you will
      no longer be able to sign-in to your identity or dapp using your Google
      Account.
    </p>
    <div class="flex w-full flex-col gap-3">
      <Button variant="primary" danger>Disconnect</Button>
      <Button variant="tertiary">Keep connected</Button>
    </div>
  </Dialog>
{/if}
