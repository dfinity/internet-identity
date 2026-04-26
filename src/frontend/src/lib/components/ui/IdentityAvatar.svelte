<script lang="ts">
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { openIdLogo } from "$lib/utils/openID";
  import { UserIcon } from "@lucide/svelte";
  import Avatar from "./Avatar.svelte";
  import AuthMethodBadge, {
    type AuthMethodBadgeVariant,
  } from "./AuthMethodBadge.svelte";

  type Props = {
    identity: LastUsedIdentity;
    size: "sm" | "lg";
  };

  let { identity, size }: Props = $props();

  // SSO entries get their own badge — never the underlying IdP's logo —
  // so the row UX matches what the user picked.
  const variant = $derived<AuthMethodBadgeVariant>(
    "sso" in identity.authMethod
      ? { type: "sso" }
      : "openid" in identity.authMethod
        ? {
            type: "openid",
            logo:
              identity.authMethod.openid.metadata !== undefined
                ? openIdLogo(
                    identity.authMethod.openid.iss,
                    // `aud` not currently tracked on `LastUsedIdentity`
                    // (see #3795) — issuer-only fallback in `findConfig`
                    // is correct for direct providers.
                    undefined,
                    identity.authMethod.openid.metadata,
                    undefined,
                  )
                : undefined,
          }
        : { type: "passkey" },
  );
</script>

<div class="relative">
  <Avatar {size}>
    <UserIcon class={size === "lg" ? "size-6" : "size-5"} />
  </Avatar>
  <AuthMethodBadge {variant} {size} />
</div>
