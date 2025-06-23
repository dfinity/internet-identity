<script lang="ts">
  import Panel from "$lib/components/ui/Panel.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { ChevronRight, InfoIcon } from "@lucide/svelte";
  import { goto } from "$app/navigation";
  import ThreeColListItem from "$lib/components/ui/ThreeColListItem.svelte";

  let identityName = "Laniakea";
  let lastUsedAccessMethod = "iCloud Keychain";
</script>

<div>
  <h1 class="text-text-primary mb-4 text-3xl font-semibold">
    Welcome, {identityName}!
  </h1>
  <h2 class="text-text-tertiary mb-12">
    Manage your identity and passkeys below.
  </h2>

  <Panel>
    <div class="p-4">
      <h3 class="text-text-primary mb-2 text-lg font-semibold">My Identity</h3>
      <h4 class="text-text-tertiary text-sm">
        Internet Identity is used to sign in securely and connect to apps with
        passkeys.
      </h4>
    </div>
    <ul>
      <ThreeColListItem>
        {#snippet colOne()}
          <h5 class="text-text-tertiary text-sm">Identity Name</h5>
        {/snippet}
        {#snippet colTwo()}
          <h5 class="text-text-primary text-sm font-semibold">
            {identityName}
          </h5>
        {/snippet}
        {#snippet colThree()}
          <Tooltip
            label="Your Identity name is currently not editable. It is only ever visible to you."
            ><InfoIcon class="text-text-primary" /></Tooltip
          >
        {/snippet}
      </ThreeColListItem>
      <ThreeColListItem href="manage/security">
        {#snippet colOne()}
          <h5 class="text-text-tertiary text-sm">Access Methods</h5>
        {/snippet}
        {#snippet colTwo()}
          <h5 class="text-text-primary text-sm font-semibold">
            {lastUsedAccessMethod}
          </h5>
        {/snippet}
        {#snippet colThree()}
          <ChevronRight class="text-text-primary" />
        {/snippet}
      </ThreeColListItem>
    </ul>
  </Panel>
</div>
