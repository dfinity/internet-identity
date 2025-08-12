<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = SVGAttributes<SVGSVGElement>;

  const { class: className, ...props }: Props = $props();
</script>

<svg
  xmlns="http://www.w3.org/2000/svg"
  {...props}
  class={["fill-none stroke-current", className]}
  viewBox="0 0 54 54"
  ><path
    vector-effect="non-scaling-stroke"
    stroke-linecap="round"
    stroke-linejoin="round"
    d="M15.311.699h-.584c-4.91 0-7.365 0-9.24.955a8.767 8.767 0 0 0-3.832 3.832C.699 7.36.699 9.816.699 14.726v.585M15.311 53.3h-.584c-4.91 0-7.365 0-9.24-.955a8.767 8.767 0 0 1-3.832-3.831c-.956-1.876-.956-4.33-.956-9.24v-.585m52.603-23.38v-.584c0-4.91 0-7.365-.956-9.24a8.767 8.767 0 0 0-3.831-3.832C46.64.7 44.185.7 39.275.7h-.585m14.612 37.99v.585c0 4.91 0 7.365-.956 9.24a8.766 8.766 0 0 1-3.831 3.832c-1.875.956-4.33.956-9.24.956h-.585M13.85 15.31v4.383m26.301-4.383v4.383m-16.073 9.06c2.338 0 4.384-2.046 4.384-4.384v-9.06m7.89 21.042a13.405 13.405 0 0 1-18.995 0"
  /></svg
>
