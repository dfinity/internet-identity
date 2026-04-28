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

  // eslint-disable-next-line svelte/no-unused-props -- `context` is consumed by the lingui-svelte preprocessor at compile time, so the runtime component never receives it
  const { id, message, values, renderNode }: Props = $props();

  const chunks = $derived.by(() => {
    void $localeStore; // Re-run on locale changes
    const translated = i18n.t({ id: id!, message, values });
    return parseMessage(translated);
  });
</script>

{#snippet renderNodeRec(children: Chunk[])}
  {#each children as chunk, i (i)}
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
