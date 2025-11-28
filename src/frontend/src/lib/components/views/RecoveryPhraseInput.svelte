<script lang="ts">
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { InfoIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { onMount } from "svelte";

  interface Props {
    value: string[];
    showAll?: boolean;
    disabled?: boolean;
  }

  let {
    value = $bindable(),
    showAll = false,
    disabled = false,
  }: Props = $props();

  let words = $derived(Array.from(value));
  let wrapperRef = $state<HTMLDivElement>();
  let dictionary = $state<string[]>();

  const handleKeyDown = (event: KeyboardEvent, index: number) => {
    if (event.code === "Backspace" && words[index].length === 0) {
      wrapperRef?.querySelectorAll("input")[index - 1]?.focus();
      event.preventDefault();
    }
    if (event.code === "Space" || event.code === "Enter") {
      wrapperRef?.querySelectorAll("input")[index + 1]?.focus();
      event.preventDefault();
    }
  };
  const handlePaste = (event: ClipboardEvent, index: number) => {
    const clipboard = (event.clipboardData?.getData("text/plain") ?? "")
      .trim()
      .split(/\s/)
      .map((word) => word.toLowerCase().replace(/[^a-z]/g, ""));
    if (clipboard.length > 0) {
      clipboard.forEach((word, i) => {
        if (index + i >= word.length) {
          return;
        }
        words[index + i] = word;
      });
      words = [...words];
      event.preventDefault();
      wrapperRef
        ?.querySelectorAll("input")
        [Math.min(index + clipboard.length - 1, words.length - 1)]?.focus();
    }
  };

  onMount(() => {
    // Lazy load dictionary so this component doesn't include it in the bundle eagerly
    import("bip39").then((bip39) => (dictionary = bip39.wordlists.english));
    // Focus on first empty input
    wrapperRef
      ?.querySelectorAll("input")
      [words.findIndex((word) => word.length === 0)]?.focus();
  });

  $effect(() => {
    if (
      words.some((word) => word.length === 0 || !dictionary?.includes(word)) ||
      words.join(" ") === value.join(" ")
    ) {
      return;
    }
    // Update value binding when recovery phrase is filled in correctly,
    // this is with a timeout so that the user can type "act" -> "actor"
    const timeout = setTimeout(() => {
      value = words;
    }, 1000);
    return () => {
      clearTimeout(timeout);
    };
  });
</script>

<div bind:this={wrapperRef} class="grid grid-cols-3 gap-3">
  {#each words as word, index}
    <label class="relative">
      <input
        inputmode="text"
        autocorrect="off"
        autocomplete="off"
        autocapitalize="off"
        spellcheck="false"
        bind:value={
          () => word,
          (v) => {
            words[index] = v.toLowerCase().replace(/[^a-z]/g, "");
            words = [...words];
          }
        }
        onkeydown={(event) => handleKeyDown(event, index)}
        onpaste={(event) => handlePaste(event, index)}
        pattern={dictionary?.join("|")}
        {disabled}
        class={[
          "peer h-7 w-full ps-8 pe-2",
          "text-text-primary bg-transparent text-base ring-0 outline-none",
          "border-border-primary rounded-full",
          "focus:not-disabled:border-fg-primary",
          "not-focus:user-invalid:!border-border-error not-focus:user-invalid:!bg-bg-error-primary/30 not-focus:user-invalid:!pe-7",
          !showAll &&
            "not-focus:valid:!text-transparent disabled:!text-transparent",
          "disabled:!text-text-disabled disabled:!bg-bg-disabled disabled:!border-border-disabled_subtle",
          word.length > 7 && "tracking-tight",
        ]}
        data-lpignore="true"
        data-1p-ignore="true"
        data-bwignore="true"
        data-form-type="other"
      />
      <span
        class={[
          "pointer-events-none absolute inset-y-0 start-0.25 flex h-7 items-center",
          "border-border-primary text-text-secondary border-r-1",
          "peer-focus:border-fg-primary peer-focus:!text-text-primary",
          "peer-not-focus:peer-user-invalid:!border-border-error peer-not-focus:peer-user-invalid:!text-text-error-primary",
          "peer-disabled:!border-border-disabled_subtle",
        ]}
        aria-hidden="true"
      >
        <span class="ms-1.5 me-1 text-xs font-semibold tabular-nums">
          {`${index + 1}`.padStart(2, "0")}
        </span>
      </span>
      <span
        class={[
          "pointer-events-none absolute inset-y-0.25 start-8 hidden",
          "text-text-primary bg-transparent text-base tracking-tight",
          "peer-disabled:!text-text-disabled",
          word.length > 0 &&
            !showAll &&
            "peer-not-focus:peer-valid:!block peer-disabled:!block",
        ]}
        aria-hidden="true"
      >
        •••••••
      </span>
      <span
        class="peer-valid:hidden peer-focus:hidden peer-disabled:hidden"
        aria-hidden="true"
      >
        <Tooltip
          label={$t`Incorrect spelling`}
          direction="up"
          align="end"
          distance="0.5rem"
        >
          <span
            class={[
              "absolute inset-y-0 end-0",
              "flex aspect-square h-full items-center justify-center rounded-full",
            ]}
          >
            <InfoIcon class="text-text-error-primary size-4" />
          </span>
        </Tooltip>
      </span>
    </label>
  {/each}
</div>
