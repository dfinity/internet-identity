<script lang="ts">
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { LogOutIcon } from "@lucide/svelte";
  import FeaturedIcon from "./FeaturedIcon.svelte";
  import IdentityListItem from "./IdentityListItem.svelte";

  type Props = {
    identity: LastUsedIdentity;
    onSignOut: () => void;
    onSignOutAndRemove: () => void;
  };

  let { identity, onSignOut, onSignOutAndRemove }: Props = $props();
</script>

<div class="flex flex-col gap-8">
  <div class="flex flex-col gap-4">
    <FeaturedIcon size="lg">
      <LogOutIcon class="size-6" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h2 class="text-text-primary text-2xl font-medium">
        {$t`Sign out from this device`}
      </h2>
      <p class="text-text-tertiary text-base">
        <Trans>
          You can either sign out and keep your identity saved for a faster
          login next time, or remove it entirely from this device.
        </Trans>
      </p>
    </div>
  </div>

  <div class="bg-bg-primary_hover flex items-center gap-3 rounded-md p-3">
    <IdentityListItem {identity} />
  </div>

  <div class="flex flex-col gap-3">
    <button onclick={onSignOut} class="btn w-full">
      {$t`Sign out and keep identity`}
    </button>
    <button onclick={onSignOutAndRemove} class="btn btn-tertiary w-full">
      {$t`Sign out and remove from device`}
    </button>
  </div>
</div>
