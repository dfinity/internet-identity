<!--
  The product's identity avatar (`IdentityAvatar`: one initial on the
  identity's own pair of colours, a hairline inside, the frosted badge of how
  it signs in), at the sizes the landing's tiles and hero need. The product's
  comes at 32 and 48px only. Pointing at the badge says how the identity
  was signed in to last.
-->
<script lang="ts">
  import { UserIcon } from "@lucide/svelte";
  import AuthMethodBadge from "$lib/components/ui/AuthMethodBadge.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
  import { identityInitial, identityPalette } from "$lib/utils/identityColors";
  import type { ClassValue } from "svelte/elements";

  interface Props {
    identity: LastUsedIdentity;
    /** In pixels. */
    size: number;
    badge?: boolean;
    class?: ClassValue;
  }

  const { identity, size, badge = false, class: className }: Props = $props();

  const palette = $derived(identityPalette(identity.identityNumber));
  const initial = $derived(identityInitial(identity.name));

  /** What the badge says when it's pointed at: how this identity was
   *  signed in to last, on this device. */
  const method = $derived.by(() => {
    const { authMethod } = identity;
    if ("sso" in authMethod) {
      return {
        label: `Signed in with ${authMethod.sso.name ?? authMethod.sso.domain}`,
        description: "Your organization's account",
      };
    }
    if ("openid" in authMethod) {
      return {
        label: "Signed in with Google",
        description: "Your Google account",
      };
    }
    return {
      label: "Signed in with a passkey",
      description: "Your face, fingerprint or screen lock",
    };
  });
</script>

<span
  aria-hidden="true"
  class={["relative inline-block shrink-0", className]}
  style:width="{size}px"
  style:height="{size}px"
>
  <span
    class="grid size-full place-items-center rounded-full leading-none font-semibold outline-1 -outline-offset-1 outline-black/8 dark:outline-white/12"
    style:background-color={palette.background}
    style:color={palette.foreground}
    style:font-size="{Math.round(size * 0.42)}px"
  >
    {#if initial !== undefined}
      {initial}
    {:else}
      <UserIcon style="width: {size * 0.45}px; height: {size * 0.45}px" />
    {/if}
  </span>
  {#if badge}
    <Tooltip label={method.label} description={method.description}>
      <AuthMethodBadge
        variant={"sso" in identity.authMethod
          ? { type: "sso" }
          : { type: "passkey" }}
        size="lg"
      />
    </Tooltip>
  {/if}
</span>
