<script lang="ts">
  import PasskeyIcon from "../icons/PasskeyIcon.svelte";
  import SsoIcon from "../icons/SsoIcon.svelte";

  export type AuthMethodBadgeVariant =
    { type: "passkey" } | { type: "openid"; logo?: string } | { type: "sso" };

  type Props = {
    variant: AuthMethodBadgeVariant;
    size: "sm" | "lg";
  };

  let { variant, size }: Props = $props();
</script>

<!-- Frosted glass: the avatar's colour shows through, blurred and a little
     more saturated, under a bright rim and a soft drop shadow. The fill stays
     at 65% so the icon keeps at least 4.5:1 over every identity palette, in
     both themes, even where the badge lies wholly over the avatar. -->
<span
  class={[
    "bg-bg-primary_alt/65 absolute flex items-center justify-center rounded-full backdrop-blur-[3px] backdrop-saturate-180",
    "shadow-[inset_0_0_0_0.5px_rgb(255_255_255/0.6),inset_0_1px_0_rgb(255_255_255/0.8),0_0_0_0.5px_rgb(0_0_0/0.07),0_1px_2px_rgb(0_0_0/0.1),0_3px_8px_rgb(0_0_0/0.08)]",
    "dark:shadow-[inset_0_0_0_0.5px_rgb(255_255_255/0.1),inset_0_1px_0_rgb(255_255_255/0.16),0_0_0_0.5px_rgb(0_0_0/0.35),0_1px_2px_rgb(0_0_0/0.3),0_3px_8px_rgb(0_0_0/0.25)]",
    size === "lg"
      ? "-inset-e-1 -bottom-1 size-6.5"
      : "-inset-e-1.25 -bottom-1.25 size-5",
  ]}
>
  {#if variant.type === "sso"}
    <SsoIcon
      class={["text-fg-secondary", size === "lg" ? "size-4.25!" : "size-3!"]}
    />
  {:else if variant.type === "openid" && variant.logo !== undefined}
    <span
      class={["text-fg-secondary", size === "lg" ? "size-4.25" : "size-3.25"]}
    >
      <!-- eslint-disable-next-line svelte/no-at-html-tags -- variant.logo is a trusted SVG string sourced from the backend canister's openid_configs -->
      {@html variant.logo}
    </span>
  {:else}
    <PasskeyIcon
      class={["text-fg-secondary", size === "lg" ? "size-4.25!" : "size-3!"]}
    />
  {/if}
</span>
