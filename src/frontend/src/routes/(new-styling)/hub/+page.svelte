<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Card from "$lib/components/ui/Card.svelte";
  import { ChevronRight, InfoIcon } from "@lucide/svelte";

  let identityName = "Laniakea";
  let lastUsedAccessMethod = "iCloud Keychain";

  const handleHoverIdentityInfo = () => {
    console.log("hovering Identity Info");
  };
</script>

<div class="text-text-primary">
  <h1 class="text-text-tertiary mb-4 text-4xl font-semibold">
    Welcome, {identityName}!
  </h1>
  <h2 class="mb-12 text-lg">
    Manage your identity, passkeys and connected apps below.
  </h2>
  <Card>
    <div class="p-4">
      <h3 class="mb-2 text-lg font-semibold">My Identity</h3>
      <h4>
        Internet Identity is used to sign in securely and connect to apps with
        passkeys.
      </h4>
    </div>
    <table class="w-full">
      <tbody>
        <tr class="border-border-tertiary border-t border-b">
          <td class="p-4">
            <h5>Identity Name</h5>
          </td>
          <td class="p-4">
            <h5 class="font-semibold">{identityName}</h5>
          </td>
          <td class="flex justify-end p-4">
            <button onmouseenter={handleHoverIdentityInfo}>
              <InfoIcon />
            </button>
          </td>
        </tr>
        <tr>
          <td colspan="3">
            <Button
              class="!h-14 w-full rounded-t-none rounded-b-2xl p-4 !text-[16px] !font-normal"
              variant="tertiary"
              href="/hub/security"
            >
              <h5 class="flex-1">Access Methods</h5>

              <h5 class="flex-1 font-semibold">{lastUsedAccessMethod}</h5>

              <span class="flex justify-end">
                <ChevronRight />
              </span>
            </Button>
          </td>
        </tr>
      </tbody>
    </table>
  </Card>
</div>
