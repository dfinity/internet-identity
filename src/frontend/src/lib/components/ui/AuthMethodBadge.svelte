<script lang="ts">
  import PasskeyIcon from "../icons/PasskeyIcon.svelte";

  type Props = {
    logo: string | undefined;
    size: "sm" | "lg";
  };

  let { logo, size }: Props = $props();
</script>

<span
  class={[
    "bg-bg-primary_alt border-border-secondary absolute flex items-center justify-center rounded-full border",
    size === "lg"
      ? "-inset-e-1 -bottom-1 size-6.5"
      : "-inset-e-1.25 -bottom-1.25 size-5",
  ]}
>
  {#if logo !== undefined}
    <span
      class={["text-fg-tertiary", size === "lg" ? "size-4.25" : "size-3.25"]}
    >
      {@html logo}
    </span>
  {:else}
    <PasskeyIcon
      class={["text-fg-tertiary", size === "lg" ? "size-4.25!" : "size-3!"]}
    />
  {/if}
</span>
