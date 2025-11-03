<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { recoverWithPhrase } from "$lib/flows/recoverWithPhraseFlow.svelte";
  import { t } from "$lib/stores/locale.store";
  import { nonNullish } from "@dfinity/utils";

  // TODO: Add word validation.
  let words = $state<Array<{ value: string }>>(
    Array.from({ length: 24 }, () => ({ value: "" })),
  );

  // Flag to prevent double-triggering recovery on multiple blur events
  let recoveryInProgress = $state(false);

  // Timeout ID for debounced submission
  let submitTimeoutId = $state<number | undefined>(undefined);

  // TODO: Use word validation instead of presence.
  const submitEnabled = $derived(
    words.every((word) => word.value.trim().length > 0),
  );

  const handleKeyDownInput = (event: KeyboardEvent, currentIndex: number) => {
    const isLastIndex = currentIndex === words.length - 1;
    if (event.key === "Enter" && isLastIndex && submitEnabled) {
      // Clear any pending timeout and submit immediately
      if (submitTimeoutId !== undefined) {
        clearTimeout(submitTimeoutId);
        submitTimeoutId = undefined;
      }
      handleRecoverWithPhrase();
      return;
    }
    if (event.key === "Enter" || event.code === "Space") {
      event.preventDefault();
      const nextInputIndex = currentIndex + 1;
      if (nextInputIndex < words.length) {
        const nextElement = document.getElementById(
          `recovery-phrase-${nextInputIndex}`,
        );
        if (nonNullish(nextElement)) {
          nextElement.focus();
        }
      }
    }

    // Clear existing timeout and start new one for auto-submit
    if (submitTimeoutId !== undefined) {
      clearTimeout(submitTimeoutId);
    }

    // Set 1 second timeout to auto-submit if all words are filled
    submitTimeoutId = window.setTimeout(() => {
      if (submitEnabled && !recoveryInProgress) {
        handleRecoverWithPhrase();
      }
    }, 1000);
  };

  const handleRecoverWithPhrase = async () => {
    if (recoveryInProgress) {
      return;
    }
    recoveryInProgress = true;

    const phraseWords = words.map((word) => word.value);
    try {
      const result = await recoverWithPhrase(phraseWords);
      // TODO: Handle success
      console.log("success", result.info);
    } catch (error) {
      // TODO: Manage error
      console.error("error", error);
    } finally {
      // Reset flag on error to allow retry
      recoveryInProgress = false;
    }
  };

  const handlePaste = (event: ClipboardEvent, currentIndex: number) => {
    event.preventDefault();

    // Get pasted text from clipboard
    const pastedText = event.clipboardData?.getData("text");
    if (!pastedText) return;

    // Uses might paste text with multiple spaces, tabs, or newlines between words.
    const pastedWords = pastedText.trim().split(/\s+/);
    // Fill inputs starting from current index.
    pastedWords.forEach((word, i) => {
      const targetIndex = currentIndex + i;
      if (targetIndex < words.length) {
        words[targetIndex].value = word;
      }
    });

    // Focus on the next empty input or the last filled input
    const nextEmptyIndex = words.findIndex(
      (word, i) => i > currentIndex && word.value.trim() === "",
    );
    const focusIndex =
      nextEmptyIndex !== -1
        ? nextEmptyIndex
        : Math.min(currentIndex + pastedWords.length, words.length - 1);

    const nextElement = document.getElementById(
      `recovery-phrase-${focusIndex}`,
    );
    // Don't set focus somewhere if all words are filled.
    if (nonNullish(nextElement) && focusIndex === words.length - 1) {
      nextElement.focus();
    }
  };

  // Cleanup timeout on component unmount
  $effect(() => {
    return () => {
      if (submitTimeoutId !== undefined) {
        clearTimeout(submitTimeoutId);
      }
    };
  });
</script>

<div
  class="flex flex-1 flex-row items-end justify-center sm:max-w-120 sm:items-center"
>
  <AuthPanel>
    <div class="flex flex-col gap-6">
      <div class="flex flex-col gap-3">
        <h1 class="text-text-primary text-2xl font-medium">
          {$t`Secure recovery`}
        </h1>
        <p class="text-text-tertiary text-sm">
          {$t`Enter your recovery phrase words in each box, spelled correctly and in order.`}
        </p>
      </div>
      <div class="flex flex-col gap-3">
        <div class="grid grid-cols-3 gap-3">
          {#each words as word, i}
            <label class="relative h-8">
              <!-- Text input -->
              <input
                type="text"
                id={`recovery-phrase-${i}`}
                bind:value={word.value}
                onkeydown={(e) => handleKeyDownInput(e, i)}
                onpaste={(e) => handlePaste(e, i)}
                class="peer text-text-primary ring-border-secondary focus:ring-border-brand h-8 w-full rounded-full border-none bg-transparent pl-10 text-base ring outline-none ring-inset focus:ring-2"
              />
              <!-- Left slot -->
              <!-- Reverse order to use "peer" class to change the border color when peer is focused -->
              <span
                class="peer peer-focus:border-border-brand text-text-secondary border-border-secondary absolute top-0 left-0 flex h-8 w-8 items-center border-r-1 px-2 text-center text-sm font-semibold peer-focus:border-r-2"
              >
                {String(i + 1).padStart(2, "0")}
              </span>
            </label>
          {/each}
        </div>
        <div class="flex flex-row gap-2">
          <Button class="w-full" variant="tertiary">{$t`Show all`}</Button>
          <Button class="w-full" variant="tertiary">{$t`Clear all`}</Button>
        </div>
      </div>
      <Button size="xl" variant="secondary">{$t`Cancel`}</Button>
    </div>
  </AuthPanel>
</div>
