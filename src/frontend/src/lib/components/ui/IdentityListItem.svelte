<script lang="ts">
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { formatRelative, t } from "$lib/stores/locale.store";
  import { getMetadataString } from "$lib/utils/openID";
  import IdentityAvatar from "./IdentityAvatar.svelte";

  type Props = {
    identity: LastUsedIdentity;
    showCreatedAt?: boolean;
  };

  let { identity, showCreatedAt }: Props = $props();
</script>

<IdentityAvatar {identity} size="sm" />
<span class="flex flex-col overflow-hidden">
  <span class="text-text-primary text-sm font-semibold">
    {identity.name ?? identity.identityNumber}
  </span>
  <span
    class="text-text-tertiary overflow-hidden text-sm text-ellipsis whitespace-nowrap"
  >
    {#if "openid" in identity.authMethod && identity.authMethod.openid.metadata !== undefined}
      <span
        >{getMetadataString(identity.authMethod.openid.metadata, "email") ??
          $t`Hidden email`}</span
      >
    {:else if "sso" in identity.authMethod}
      {@const sso = identity.authMethod.sso}
      <span>{sso.email ?? sso.name ?? sso.domain}</span>
    {:else}
      <span>
        {$t`Passkey`}
        {#if showCreatedAt === true && identity.createdAtMillis !== undefined}
          |
          {$t`Created ${$formatRelative(new Date(identity.createdAtMillis), {
            style: "long",
          })}`}
        {/if}
      </span>
    {/if}
  </span>
</span>
