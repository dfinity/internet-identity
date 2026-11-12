<script lang="ts">
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { identityInitial, identityPalette } from "$lib/utils/identityColors";
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

  // The identity's initial on colours of its own, so that the identities on
  // a device tell apart; the person icon only for a name without letters.
  const palette = $derived(identityPalette(identity.identityNumber));
  const initial = $derived(identityInitial(identity.name));

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
  <!-- Decorative: the identity's name is always written beside it. The fill
       reaches the edge under a clear border; a hairline drawn inside keeps a
       pale ground from melting into a pale page, and a dark one into a dark
       page. The one letter stands in the middle: at most the foot of a wide
       one reaches under the badge's rim. -->
  <Avatar
    {size}
    aria-hidden="true"
    class="outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12"
    style="background-color: {palette.background}; color: {palette.foreground}; border-color: transparent;"
  >
    {#if initial !== undefined}
      {initial}
    {:else}
      <UserIcon class={size === "lg" ? "size-6" : "size-5"} />
    {/if}
  </Avatar>
  <AuthMethodBadge {variant} {size} />
</div>
