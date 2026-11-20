<script lang="ts">
  import type { Snippet } from "svelte";
  import { provideSurface } from "./Elevated.svelte";

  interface Props {
    /** The level everything inside counts as standing on, whatever it lies
     *  on itself. Paints nothing. */
    level: number;
    children: Snippet;
  }

  const { level, children }: Props = $props();

  // Fixed for the component's life, as an Elevated's level is.
  // svelte-ignore state_referenced_locally
  provideSurface(level);
</script>

{@render children()}
