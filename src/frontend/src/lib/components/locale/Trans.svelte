<script lang="ts">
  import { i18n } from "@lingui/core";
  import { localeStore } from "$lib/stores/locale.store";
  import type { Snippet } from "svelte";
  import { parseMessage, type Chunk } from "$lib/components/locale/utils";

  interface Props {
    id?: string;
    message?: string;
    values?: Record<string, unknown>;
    renderNode?: Snippet<[Snippet, number]>;
    children?: Snippet;
    context?: string;
  }

  const { id, message, values, renderNode }: Props = $props();

  const chunks = $derived.by(() => {
    $localeStore; // Re-run on locale changes
    const translated = i18n.t({ id: id!, message, values });
    return parseMessage(translated);
  });
</script>

{#snippet renderNodeRec(children: Chunk[])}
  {#each children as chunk}
    {#if chunk.type === "text"}
      {chunk.text}
    {:else}
      {#snippet childSnippet()}
        {#if chunk.children.length > 0}
          {@render renderNodeRec(chunk.children)}
        {/if}
      {/snippet}
      {@render renderNode?.(childSnippet, chunk.index)}
    {/if}
  {/each}
{/snippet}

{@render renderNodeRec(chunks)}
