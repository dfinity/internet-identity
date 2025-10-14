interface TextChunk {
  type: "text";
  text: string;
}

interface NodeChunk {
  type: "node";
  index: number;
  children: Chunk[];
}

export type Chunk = TextChunk | NodeChunk;

export const parseMessage = (input: string): Chunk[] => {
  const stack: NodeChunk[] = [];
  const root: Chunk[] = [];
  let textBuffer = "";

  const flushText = () => {
    if (textBuffer.length > 0) {
      const target = stack.length > 0 ? stack[stack.length - 1].children : root;
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

    if (selfClosing.length > 0) {
      const target = stack.length > 0 ? stack[stack.length - 1].children : root;
      target.push({ type: "node", index, children: [] });
      continue;
    }

    if (slash.length === 0) {
      const node: NodeChunk = { type: "node", index, children: [] };
      const target = stack.length > 0 ? stack[stack.length - 1].children : root;
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
