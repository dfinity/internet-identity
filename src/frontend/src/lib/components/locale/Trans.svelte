<script lang="ts">
  import { i18n } from "@lingui/core";
  import { localeStore } from "$lib/stores/locale.store";
  import type { Snippet } from "svelte";

  interface Props {
    id?: string;
    message?: string;
    values?: Record<string, unknown>;
    renderNode?: Snippet<[Snippet, number]>;
    children?: Snippet;
    context?: string;
  }

  const { id, message, values, renderNode }: Props = $props();

  type TextChunk = { type: "text"; text: string };
  type NodeChunk = { type: "node"; index: number; children: Chunk[] };
  type Chunk = TextChunk | NodeChunk;

  // Parse <n>, </n>, and <n/> placeholders into a tree
  const parseMessage = (input: string): Chunk[] => {
    const stack: NodeChunk[] = [];
    const root: Chunk[] = [];
    let textBuffer = "";

    const flushText = () => {
      if (textBuffer.length) {
        const target = stack.length ? stack[stack.length - 1].children : root;
        target.push({ type: "text", text: textBuffer });
        textBuffer = "";
      }
    };

    const tagRegex = /<(\/?)(\d+)(\/?)>/g;
    let lastIndex = 0;
    let match: RegExpExecArray | null;

    while ((match = tagRegex.exec(input)) !== null) {
      textBuffer += input.slice(lastIndex, match.index);
      lastIndex = tagRegex.lastIndex;

      const [, slash, numStr, selfClosing] = match;
      const index = Number(numStr);

      flushText();

      if (selfClosing) {
        const target = stack.length ? stack[stack.length - 1].children : root;
        target.push({ type: "node", index, children: [] });
        continue;
      }

      if (!slash) {
        const node: NodeChunk = { type: "node", index, children: [] };
        const target = stack.length ? stack[stack.length - 1].children : root;
        target.push(node);
        stack.push(node);
      } else {
        stack.pop();
      }
    }

    textBuffer += input.slice(lastIndex);
    flushText();
    return root;
  };

  const chunks = $derived.by(() => {
    $localeStore; // Re-run on locale changes
    const rawMessage = i18n.t({ id: id!, message, values });
    return parseMessage(String(rawMessage));
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
