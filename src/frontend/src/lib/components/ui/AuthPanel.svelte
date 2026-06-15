<script lang="ts" module>
  import { getContext } from "svelte";

  export const isInsideAuthPanel = (): boolean =>
    getContext("inAuthPanel") === true;
</script>

<script lang="ts">
  import { setContext } from "svelte";
  import type { Snippet } from "svelte";

  setContext("inAuthPanel", true);

  const { children }: { children?: Snippet } = $props();
</script>

{@render children?.()}
