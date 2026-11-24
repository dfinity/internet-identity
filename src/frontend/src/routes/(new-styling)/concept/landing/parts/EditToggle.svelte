<!--
  Edit, then Done: turns the identities' remove buttons on and off, as
  "Edit identities" does on today's landing page. Removing only forgets an
  identity on this device.
-->
<script lang="ts">
  import { PencilIcon } from "@lucide/svelte";
  import type { ClassValue } from "svelte/elements";
  import type { Playground } from "../playground.svelte";

  const { p, class: className }: { p: Playground; class?: ClassValue } =
    $props();
</script>

<button
  type="button"
  onclick={() => (p.editing = !p.editing)}
  aria-pressed={p.editing}
  class={[
    "btn btn-tertiary btn-sm text-text-tertiary hover:text-text-primary rounded-full font-medium",
    className,
  ]}
>
  {#if p.editing}
    Done
  {:else}
    <PencilIcon class="size-3.5" aria-hidden="true" />
    Edit identities
  {/if}
</button>
