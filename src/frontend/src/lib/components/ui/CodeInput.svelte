<script lang="ts">
  import Input from "$lib/components/ui/Input.svelte";
  import type { HTMLAttributes } from "svelte/elements";

  interface Props extends HTMLAttributes<HTMLDivElement> {
    element: HTMLInputElement | undefined;
    value: string;
    length: number;
  }

  let {
    length,
    element = $bindable(),
    value = $bindable(),
    class: className,
    ...props
  }: Props = $props();

  let code = $state<string[]>([]);
  let inputRefs = $state<HTMLInputElement[]>([]);

  const handleKeyDown = (event: KeyboardEvent, index: number) => {
    if (event.code === "Backspace") {
      if ((code[index] ?? "").length === 0) {
        inputRefs[index - 1]?.focus();
      }
      code[index] = "";
      event.preventDefault();
    }
    if (event.code === "ArrowLeft") {
      inputRefs[index - 1]?.focus();
      event.preventDefault();
    }
    if (event.code === "ArrowRight" || event.code === "Space") {
      inputRefs[index + 1]?.focus();
      event.preventDefault();
    }
  };
  const handlePaste = (event: ClipboardEvent) => {
    code =
      event.clipboardData
        ?.getData("text/plain")
        .replace(/\D/g, "")
        .slice(0, length)
        .split("") ?? [];
    inputRefs[code.length - 1]?.focus();
    event.preventDefault();
  };

  // Keep value and code in sync
  $effect(() => {
    value = code.join("");
  });
  $effect(() => {
    if (value !== code.join("")) {
      code = value.split("");
    }
  });
  $effect(() => {
    element = inputRefs[0];
  });
</script>

<div {...props} class={["flex gap-2", className]}>
  {#each { length } as _, index}
    <Input
      bind:element={
        () => inputRefs[index], (element) => (inputRefs[index] = element)
      }
      size="md"
      class="w-0 flex-1"
      inputClass="w-full text-center text-lg font-bold h-13"
      inputmode="numeric"
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      bind:value={
        () => `${code[index] ?? ""}`,
        (value) => {
          if (/\d/.test(value.slice(-1))) {
            code[index] = value.slice(-1);
            inputRefs[index + 1]?.focus();
          }
        }
      }
      onkeydown={(event) => handleKeyDown(event, index)}
      onpaste={handlePaste}
    />
  {/each}
</div>
