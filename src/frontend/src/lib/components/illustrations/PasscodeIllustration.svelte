<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = SVGAttributes<SVGSVGElement>;

  const { class: className, ...props }: Props = $props();
</script>

<svg
  {...props}
  class={["fill-none stroke-current", className]}
  viewBox="0 0 53 53"
  xmlns="http://www.w3.org/2000/svg"
  ><path
    stroke-linecap="round"
    vector-effect="non-scaling-stroke"
    d="M20.873 9.586 9.73 20.729M42.873 31.586 31.73 42.729M18.873 26.158h-7.429M41.16 26.158h-7.43"
  /><circle
    cx="4.302"
    cy="26.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /><circle
    cx="26.302"
    cy="26.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /><circle
    cx="48.302"
    cy="26.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /><circle
    cx="4.302"
    cy="48.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /><circle
    cx="26.302"
    cy="48.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /><circle
    cx="48.302"
    cy="48.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /><circle
    cx="4.302"
    cy="4.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /><circle
    cx="26.302"
    cy="4.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /><circle
    cx="48.302"
    cy="4.158"
    r="3.5"
    vector-effect="non-scaling-stroke"
  /></svg
>
