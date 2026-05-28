<script lang="ts">
  import type { Snippet } from "svelte";
  import CopyCommandRow from "./CopyCommandRow.svelte";

  interface Props {
    /** Text content rendered above the install command. */
    children: Snippet;
    /** Install command shown in the click-to-copy terminal row. */
    installCommand: string;
  }

  const { children, installCommand }: Props = $props();
</script>

<div class="flex flex-col">
  <div
    class="text-text-secondary border-border-secondary rounded-t-lg border border-b-0 p-4 text-sm"
  >
    {@render children()}
  </div>
  <div class="rounded-t-none [&_button]:rounded-t-none">
    <CopyCommandRow command={installCommand} />
  </div>
</div>
