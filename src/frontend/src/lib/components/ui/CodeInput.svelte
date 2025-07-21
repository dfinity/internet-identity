<script lang="ts">
  import Input from "$lib/components/ui/Input.svelte";
  import type { HTMLAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";

  interface Props extends HTMLAttributes<HTMLDivElement> {
    element: HTMLInputElement | undefined;
    value: string;
    length: number;
    hint?: string;
    error?: string;
    disabled?: boolean;
  }

  let {
    length,
    element = $bindable(),
    value = $bindable(),
    hint,
    error,
    disabled,
    class: className,
    ...props
  }: Props = $props();

  let inputRefs = $state<HTMLInputElement[]>([]);

  const handleKeyDown = (event: KeyboardEvent, index: number) => {
    if (event.code === "Backspace") {
      inputRefs[index - 1]?.focus();
      const code = value.padEnd(length, "").split("");
      code[index] = " ";
      value = code.join("").trimEnd();
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
    value =
      event.clipboardData
        ?.getData("text/plain")
        .replace(/[^\d ]/g, "")
        .slice(0, length) ?? "";
    inputRefs[value.length - 1]?.focus();
    event.preventDefault();
  };

  $effect(() => {
    element = inputRefs[0];
  });
</script>

<div {...props} class={["flex flex-col gap-1", className]}>
  <div class="flex gap-2">
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
        aria-label={`Code input ${index}`}
        spellcheck="false"
        bind:value={
          () => value.slice(index, index + 1).trim(),
          (v) => {
            if (/[\d ]/.test(v.slice(-1))) {
              const code = value.padEnd(length, "").split("");
              code[index] = v.slice(-1);
              value = code.join("").trimEnd();
              inputRefs[index + 1]?.focus();
            }
          }
        }
        onkeydown={(event) => handleKeyDown(event, index)}
        onpaste={handlePaste}
        errorBorder={nonNullish(error)}
        {disabled}
      />
    {/each}
  </div>
  {#if nonNullish(error) || nonNullish(hint)}
    <div
      class={[
        "text-sm",
        nonNullish(error) ? "text-text-error-primary" : "text-text-tertiary",
      ]}
    >
      {error ?? hint}
    </div>
  {/if}
</div>
