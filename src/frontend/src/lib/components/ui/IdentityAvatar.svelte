<script lang="ts">
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { openIdLogo } from "$lib/utils/openID";
  import { UserIcon } from "@lucide/svelte";
  import Avatar from "./Avatar.svelte";
  import AuthMethodBadge from "./AuthMethodBadge.svelte";

  type Props = {
    identity: LastUsedIdentity;
    size: "sm" | "lg";
  };

  let { identity, size }: Props = $props();

  const logo = $derived(
    "openid" in identity.authMethod &&
      identity.authMethod.openid.metadata !== undefined
      ? openIdLogo(
          identity.authMethod.openid.iss,
          // `sub` and `aud` not currently tracked on `LastUsedIdentity`;
          // see #3795. Falls back to issuer-only matching in findConfig —
          // correct for direct providers, imprecise for SSO until that's
          // fixed.
          identity.authMethod.openid.sub,
          undefined,
          identity.authMethod.openid.metadata,
        )
      : undefined,
  );
</script>

<div class="relative">
  <Avatar {size}>
    <UserIcon class={size === "lg" ? "size-6" : "size-5"} />
  </Avatar>
  <AuthMethodBadge {logo} {size} />
</div>
