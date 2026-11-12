<script lang="ts">
  interface Props {
    /** A risk shows red; a step to take, or one waiting, amber. */
    tone: "risk" | "action";
  }

  const { tone }: Props = $props();
</script>

<!-- A dot on an avatar's top corner, where the sign-in badge (bottom corner)
     leaves it room: the account has something for you. It cuts itself out of
     what the avatar stands on (`--ii-dot-backdrop`, the canvas by default), as
     a tile's count does. Decorative: whoever shows it says so in a label. -->
<span
  aria-hidden="true"
  class={[
    "pointer-events-none absolute -end-0.5 -top-0.5 size-2.5 rounded-full ring-2 ring-[color:var(--ii-dot-backdrop,var(--ii-surface-3))]",
    tone === "risk" ? "bg-bg-error-solid" : "bg-bg-warning-solid",
  ]}
></span>
