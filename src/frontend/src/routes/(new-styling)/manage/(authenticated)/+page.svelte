<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Card from "$lib/components/ui/Card.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { ChevronRight, InfoIcon } from "@lucide/svelte";
  import { goto } from "$app/navigation";

  let identityName = "Laniakea";
  let lastUsedAccessMethod = "iCloud Keychain";

  const handleHoverIdentityInfo = () => {
    console.log("hovering Identity Info");
  };
</script>

<div>
  <h1 class="text-text-primary mb-4 text-3xl font-semibold">
    Welcome, {identityName}!
  </h1>
  <h2 class="text-text-tertiary mb-12">
    Manage your identity and passkeys below.
  </h2>
  <Card>
    <div class="p-4">
      <h3 class="text-text-primary mb-2 text-lg font-semibold">My Identity</h3>
      <h4 class="text-text-tertiary text-sm">
        Internet Identity is used to sign in securely and connect to apps with
        passkeys.
      </h4>
    </div>
    <table class="w-full">
      <tbody>
        <tr class="border-border-tertiary border-t border-b">
          <td class="p-4">
            <h5 class="text-text-tertiary text-sm">Identity Name</h5>
          </td>
          <td class="p-4">
            <h5 class="text-text-primary text-sm font-semibold">
              {identityName}
            </h5>
          </td>
          <td class="text-text-primary flex justify-end p-4"
            ><Tooltip
              label="Your Identity name is currently not editable. It is only ever visible to you."
              ><InfoIcon /></Tooltip
            ></td
          >
        </tr>
        <tr
          tabindex="0"
          role="button"
          class="border-border-secondary text-fg-primary not-disabled:hover:bg-bg-primary_hover disabled:border-border-disabled disabled:text-fg-disabled focus-visible:ring-offset-bg-primary focus-visible:ring-focus-ring h-10 cursor-pointer gap-1.5 px-3.5 text-sm outline-none focus-visible:ring-2 focus-visible:ring-offset-2"
          onclick={() => goto("/hub/security")}
          onkeydown={(e) => {
            if (e.key === "Enter" || e.key === " ") goto("/hub/security");
          }}
        >
          <td class="text-text-tertiary rounded-bl-2xl p-4 text-sm">
            <h5>Access Methods</h5>
          </td>
          <td class="p-4">
            <h5 class="font-semibold">{lastUsedAccessMethod}</h5>
          </td>
          <td class="flex justify-end p-4">
            <ChevronRight />
          </td>
        </tr>
      </tbody>
    </table>
  </Card>
</div>
