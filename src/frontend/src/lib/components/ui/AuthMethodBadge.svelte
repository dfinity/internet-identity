<script lang="ts">
  import PasskeyIcon from "../icons/PasskeyIcon.svelte";
  import SsoIcon from "../icons/SsoIcon.svelte";

  export type AuthMethodBadgeVariant =
    | { type: "passkey" }
    | { type: "openid"; logo?: string }
    | { type: "sso" };

  type Props = {
    variant: AuthMethodBadgeVariant;
    size: "sm" | "lg";
  };

  let { variant, size }: Props = $props();
</script>

<span
  class={[
    "bg-bg-primary_alt border-border-secondary absolute flex items-center justify-center rounded-full border",
    size === "lg"
      ? "-inset-e-1 -bottom-1 size-6.5"
      : "-inset-e-1.25 -bottom-1.25 size-5",
  ]}
>
  {#if variant.type === "sso"}
    <SsoIcon
      class={["text-fg-tertiary", size === "lg" ? "size-4.25!" : "size-3!"]}
    />
  {:else if variant.type === "openid" && variant.logo !== undefined}
    <span
      class={["text-fg-tertiary", size === "lg" ? "size-4.25" : "size-3.25"]}
    >
      <!-- eslint-disable-next-line svelte/no-at-html-tags -- variant.logo is a trusted SVG string sourced from the backend canister's openid_configs -->
      {@html variant.logo}
    </span>
  {:else}
    <PasskeyIcon
      class={["text-fg-tertiary", size === "lg" ? "size-4.25!" : "size-3!"]}
    />
  {/if}
</span>
