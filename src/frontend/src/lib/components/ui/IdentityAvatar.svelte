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
          // `aud` not currently tracked on `LastUsedIdentity` (see
          // #3795) — issuer-only fallback in `findConfig` is correct
          // for direct providers, imprecise for SSO until that's
          // fixed. `ssoDomain` is unknown here for the same reason,
          // so SSO-linked last-used identities render with the
          // underlying IdP's logo.
          undefined,
          identity.authMethod.openid.metadata,
          undefined,
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
