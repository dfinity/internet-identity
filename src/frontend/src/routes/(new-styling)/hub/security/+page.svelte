<script lang="ts">
  import Card from "$lib/components/ui/Card.svelte";
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import { Plus, Unlink } from "@lucide/svelte";
</script>

<div class="text-text-primary">
  <h1 class="mb-4 text-4xl">Security</h1>
  <h2 class="mb-12 text-lg">
    Settings and recommendations to keep your identity secure
  </h2>
  <Card>
    <div class="flex gap-5 p-4">
      <div>
        <h3 class="mb-2 text-lg font-semibold">Access methods</h3>
        <h4>Manage your passkeys, security keys and linked accounts.</h4>
      </div>

      <div>
        <ButtonOrAnchor
          class="bg-bg-brand-solid text-text-primary-inversed top-0 flex items-center gap-1 rounded-sm px-3.5 py-2 font-semibold"
          >Add <Plus size="1.25rem" /></ButtonOrAnchor
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
            <h5>"asdf"</h5>
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
            <h5>"asdf"</h5>
          </td>
          <td class="flex justify-end p-4"
            ><ButtonOrAnchor href="/hub/security">
              <Unlink class="stroke-fg-error-secondary" />
            </ButtonOrAnchor></td
          ></tr
        >
      </tbody>
    </table>
  </Card>
</div>
