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
          <td class="text-text-primary flex justify-end p-4">
            <InfoIcon />
          </td>
        </tr>
        <tr>
          <td colspan="3">
            <Button
              class="!h-14 w-full !gap-0 rounded-t-none rounded-b-2xl p-4 !text-[16px] !font-normal"
              variant="tertiary"
              href="/hub/security"
            >
              <h5 class="text-text-tertiary flex-1 text-sm">Access Methods</h5>

              <h5 class="text-text-primary flex-1 text-sm font-semibold">
                {lastUsedAccessMethod}
              </h5>

              <span class="text-text-primary flex justify-end">
                <ChevronRight />
              </span>
            </Button>
          </td>
        </tr>
      </tbody>
    </table>
  </Card>
</div>
