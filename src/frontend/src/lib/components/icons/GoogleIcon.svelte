<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = SVGAttributes<SVGSVGElement> & {
    size?: string;
  };

  const { size = "1.25rem", ...props }: Props = $props();
</script>

<svg viewBox="0 0 20 20" width={size} height={size} {...props}>
  <path
    d="M10 .83a9.15 9.15 0 0 0-8.18 13.28c1.5 3 4.6 5.06 8.18 5.06 2.47 0 4.55-.82 6.07-2.22a8.95 8.95 0 0 0 2.73-6.74c0-.65-.06-1.28-.17-1.88H10v3.55h4.93a4.23 4.23 0 0 1-1.84 2.76 5.47 5.47 0 0 1-8.22-2.9h-.01A5.5 5.5 0 0 1 10 4.48a5 5 0 0 1 3.5 1.37l2.63-2.63A8.8 8.8 0 0 0 10 .83Z"
    class="fill-current"
  />
</svg>
