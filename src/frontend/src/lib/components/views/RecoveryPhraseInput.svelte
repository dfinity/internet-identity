<script lang="ts">
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { EyeIcon, EyeOffIcon, InfoIcon, ListXIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { onMount } from "svelte";
  import type { SvelteHTMLElements } from "svelte/elements";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  const EMPTY_PHRASE = Array.from({ length: 24 }).map(() => "");

  type Props = {
    value?: string[];
    onSubmit: (recoveryPhrase: string[]) => Promise<void>;
  } & SvelteHTMLElements["div"];

  let {
    value: initialValue,
    onSubmit,
    class: className,
    ...props
  }: Props = $props();

  let value = $state(initialValue ?? EMPTY_PHRASE);
  let wrapperRef = $state<HTMLFieldSetElement>();
  let dictionary = $state<string[]>();
  let isSubmitting = $state(false);
  let showValues = $state(false);

  const isEmpty = $derived(value.every((word) => word.length === 0));
  const isValid = $derived(
    value.every(
      (word) => dictionary !== undefined && dictionary.includes(word),
    ),
  );
  const inputPattern = $derived(dictionary?.join("|"));

  /**
   * Focus on an input field, noop if not found
   *
   * @param index of the input field to focus
   */
  const focusInput = (index: number) => {
    wrapperRef?.querySelectorAll("input")[index]?.focus();
  };
  /**
   * Mask input values to keep only allowed characters
   *
   * @param value with disallowed characters
   * @return lower case alpha string
   */
  const maskInput = (value: string) =>
    value.toLowerCase().replace(/[^a-z]/g, "");
  /**
   * Switch focus between inputs when user presses certain keys
   *
   * @param event to identify which key was pressed
   * @param index of the currently focused input field
   */
  const handleKeyDown = (event: KeyboardEvent, index: number) => {
    if (event.key === "Backspace" && value[index].length === 0) {
      event.preventDefault();
      focusInput(index - 1);
    }
    if (event.key === " " || event.key === "Enter") {
      event.preventDefault();
      focusInput(index + 1);
    }
  };
  /**
   * Handle pasting clipboard content, split it into multiple words based on
   * whitespace and fills inputs with these words starting from current input.
   *
   * @param event that contains clipboard data
   * @param index of the currently focused input field
   */
  const handlePaste = (event: ClipboardEvent, index: number) => {
    const clipboard = (event.clipboardData?.getData("text/plain") ?? "")
      .trim()
      .split(/\s/)
      .map((word) => word.toLowerCase().replace(/[^a-z]/g, ""));
    if (clipboard.length > 0) {
      clipboard.forEach((word, i) => {
        if (index + i >= value.length) {
          return;
        }
        value[index + i] = word;
      });
      event.preventDefault();
      focusInput(Math.min(index + clipboard.length - 1, value.length - 1));
    }
  };
  /**
   * Submit and disable all fields during submission
   */
  const handleSubmit = async () => {
    try {
      isSubmitting = true;
      await onSubmit(value);
    } finally {
      isSubmitting = false;
    }
  };

  onMount(() => {
    // Lazy load dictionary so this component doesn't include it in the bundle eagerly
    import("bip39").then((bip39) => (dictionary = bip39.wordlists.english));
    // Focus on first empty input field, else fallback to first input
    focusInput(
      Math.max(
        value.findIndex((word) => word.length === 0),
        0,
      ),
    );
  });
</script>

<fieldset
  bind:this={wrapperRef}
  disabled={isSubmitting}
  class="flex flex-col gap-5"
>
  <!-- Input grid -->
  <div
    {...props}
    class={[
      "grid grid-cols-3 overflow-hidden",
      "border-border-secondary rounded-md border-1",
      "bg-bg-tertiary",
      className,
    ]}
  >
    {#each value as word, index}
      {@const position = index + 1}
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
              value[index] = maskInput(v);
            }
          }
          onkeydown={(event) => handleKeyDown(event, index)}
          onpaste={(event) => handlePaste(event, index)}
          pattern={inputPattern}
          class={[
            // Base
            "peer h-7 w-full ps-8 pe-2 text-base",
            "ring-bg-tertiary border-none ring-1 outline-none",
            "text-text-primary bg-bg-primary",
            // Focus
            "focus:bg-bg-active",
            // Invalid
            "invalid:not-focus:bg-bg-error-primary invalid:not-focus:text-text-error-primary",
            "sm:not-focus:invalid:pe-7",
            // Disabled
            "disabled:!text-text-disabled disabled:!bg-bg-disabled",
            // Hide values
            !showValues &&
              "not-focus:valid:!text-transparent disabled:!text-transparent",
            // Value too long
            word.length > 7 && "tracking-tight",
            // iOS scroll on focus fix
            "input-noscroll",
          ]}
          aria-label={$t`Word ${position}`}
          data-lpignore="true"
          data-1p-ignore="true"
          data-bwignore="true"
          data-form-type="other"
        />
        <!-- Word index label -->
        <span
          class={[
            // Base
            "pointer-events-none absolute inset-y-0 start-0.25 flex h-7 items-center",
            "text-text-secondary",
            // Focus
            "peer-focus:!text-text-primary",
            // Invalid
            "peer-invalid:peer-not-focus:!text-text-error-primary",
          ]}
          aria-hidden="true"
        >
          <span class="ms-1.5 me-1 text-xs font-semibold tabular-nums">
            {`${index + 1}`.padStart(2, "0")}
          </span>
        </span>
        <!-- Placeholder shown when value is hidden -->
        <span
          class={[
            "pointer-events-none absolute inset-y-0.25 start-8 hidden",
            "text-text-primary bg-transparent text-base tracking-tight",
            "peer-disabled:!text-text-disabled",
            word.length > 0 &&
              !showValues &&
              "peer-not-focus:peer-valid:!block peer-disabled:!block",
          ]}
          aria-hidden="true"
        >
          •••••••
        </span>
        <!-- Error icon with tooltip -->
        <span
          class="peer-valid:hidden peer-focus:hidden max-sm:hidden"
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
  <!-- Controls and submit -->
  <div class={["flex gap-3", "max-sm:flex-row", "sm:flex-col"]}>
    <div class={["flex flex-row items-center gap-3"]}>
      <div class="relative flex flex-1 flex-row justify-center">
        <button
          onclick={() => (showValues = !showValues)}
          onpointerdown={(event) => {
            if (event.pointerType === "touch") {
              event.preventDefault();
            }
          }}
          disabled={isEmpty}
          class={[
            "btn btn-tertiary relative",
            "max-sm:btn-icon max-sm:after:absolute max-sm:after:-inset-2.5",
            "sm:flex-1",
          ]}
        >
          {#if showValues}
            <EyeOffIcon />
            <span>{$t`Hide all`}</span>
          {:else}
            <EyeIcon />
            <span>{$t`Show all`}</span>
          {/if}
        </button>
      </div>
      <div class="relative flex flex-1 flex-row justify-center">
        <button
          onclick={() => (value = EMPTY_PHRASE)}
          onpointerdown={(event) => {
            if (event.pointerType === "touch") {
              event.preventDefault();
              focusInput(0);
            }
          }}
          disabled={isEmpty}
          class={[
            "btn btn-tertiary relative",
            "max-sm:btn-icon max-sm:after:absolute max-sm:after:-inset-2.5",
            "sm:flex-1",
          ]}
        >
          <ListXIcon />
          <span>{$t`Clear all`}</span>
        </button>
      </div>
    </div>
    <button
      onclick={handleSubmit}
      disabled={!isValid}
      class={["btn", "max-sm:ms-auto max-sm:px-10", "sm:btn-xl"]}
    >
      {#if isSubmitting}
        <ProgressRing />
      {/if}
      <span>{$t`Submit`}</span>
    </button>
  </div>
</fieldset>
