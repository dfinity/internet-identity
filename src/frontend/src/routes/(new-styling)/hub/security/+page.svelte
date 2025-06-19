<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Card from "$lib/components/ui/Card.svelte";
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import { Plus, Unlink } from "@lucide/svelte";

  const handleUnlinkCredential = () => {
    console.log("unlink credential"); //TODO
  };

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
            ><Button variant="tertiary" onclick={handleUnlinkCredential}>
              <Unlink class="stroke-fg-error-secondary" />
            </Button></td
          ></tr
        >
      </tbody>
    </table>
  </Card>
</div>
