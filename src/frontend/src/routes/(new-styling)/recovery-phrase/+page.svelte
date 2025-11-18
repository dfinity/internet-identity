<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import {
    IdentityNotFoundError,
    recoverWithPhrase,
  } from "$lib/flows/recoverWithPhraseFlow.svelte";
  import {
    authenticatedStore,
    authenticationStore,
  } from "$lib/stores/authentication.store";
  import { t } from "$lib/stores/locale.store";
  import { nanosToMillis } from "$lib/utils/time";
  import { nonNullish } from "@dfinity/utils";
  import { goto } from "$app/navigation";
  import { InfoIcon } from "@lucide/svelte";
  import type { IdentityInfo } from "$lib/generated/internet_identity_types";
  import type { DelegationIdentity } from "@icp-sdk/core/identity";
  import { wordlists } from "bip39";
  import SuccessfulRecovery from "./components/SuccessfulRecovery.svelte";
  import RecoveryError from "./components/RecoveryError.svelte";
  import IdentityNotFound from "./components/IdentityNotFound.svelte";
  import CancelRecovery from "./components/CancelRecovery.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { throwCanisterError } from "$lib/utils/utils";
  import { get } from "svelte/store";

  type RecoveryWord = {
    value: string;
    isValid: boolean;
    showContent: boolean;
  };

  const englishWordList = (wordlists.english as string[]) ?? [];
  const bip39EnglishWords = new Set(
    englishWordList.map((word) => word.toLowerCase()),
  );

  let words = $state<Array<RecoveryWord>>(
    Array.from({ length: 24 }, () => ({
      value: "",
      isValid: true,
      showContent: false,
    })),
  );

  let showAll = $state(false);

  // Flag to prevent double-triggering recovery on multiple blur events
  let recoveryInProgress = $state(false);

  // Timeout ID for debounced submission
  let submitTimeoutId = $state<number | undefined>(undefined);

  let recoveredIdentityData = $state<
    | {
        info: IdentityInfo;
        identityNumber: bigint;
        identity: DelegationIdentity;
      }
    | undefined
  >(undefined);

  let continueInProgress = $state(false);
  let recoveryError = $state(false); // Recovery phrase fails checksum validation
  let identityNotFoundError = $state(false);
  // When this is `true`, the auto-submit is disabled, and the user has to manually submit the recovery phrase.
  let manualSubmitRequired = $state(false);
  let showCancelDialog = $state(false);

  const submitEnabled = $derived(
    words.every((word) => word.value.trim().length > 0 && word.isValid),
  );
  const hasAnyWord = $derived(
    words.some((word) => word.value.trim().length > 0),
  );

  const validateWord = (index: number) => {
    const entry = words[index];
    if (!entry) {
      return;
    }

    const normalized = entry.value.trim().toLowerCase();
    entry.isValid =
      normalized.length === 0 || bip39EnglishWords.has(normalized);
  };

  const handleKeyDownInput = (event: KeyboardEvent, currentIndex: number) => {
    const currentWord = words[currentIndex];
    // Reset validity state when typing
    if (currentWord && event.key !== "Enter") {
      currentWord.isValid = true;
    }

    // Validate word when entering
    if (event.key === "Enter") {
      validateWord(currentIndex);
    }

    if (!manualSubmitRequired) {
      // Clear any pending timeout and submit immediately if last index and submit is enabled
      const isLastIndex = currentIndex === words.length - 1;
      if (event.key === "Enter" && isLastIndex && submitEnabled) {
        clearTimeout(submitTimeoutId);
        submitTimeoutId = undefined;
        handleRecoverWithPhrase();
        return;
      }
    }

    // Focus next input if not last index
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

    if (!manualSubmitRequired) {
      // Clear existing timeout and start new one for auto-submit
      clearTimeout(submitTimeoutId);
      // Set 1 second timeout to auto-submit if all words are filled
      submitTimeoutId = window.setTimeout(() => {
        if (submitEnabled && !recoveryInProgress) {
          handleRecoverWithPhrase();
        }
      }, 1000);
    }
  };

  const handleRecoverWithPhrase = async () => {
    if (recoveryInProgress) {
      return;
    }
    // We need to validate all words because it might be triggered by a timeout when the isInvalid state is not yet updated.
    words.forEach((_, index) => validateWord(index));

    if (!submitEnabled) {
      return;
    }

    // Set the flag to prevent multiple submissions
    recoveryInProgress = true;
    recoveryError = false;
    identityNotFoundError = false;
    recoveredIdentityData = undefined;

    const phraseWords = words.map((word) => word.value.trim());
    try {
      const result = await recoverWithPhrase(phraseWords);
      recoveredIdentityData = result;
    } catch (error) {
      recoveredIdentityData = undefined;
      if (error instanceof IdentityNotFoundError) {
        identityNotFoundError = true;
      } else {
        recoveryError = true;
      }
    } finally {
      clearTimeout(submitTimeoutId);
      submitTimeoutId = undefined;
      // Reset flag on error to allow retry
      recoveryInProgress = false;
    }
  };

  const resetRecoveryState = () => {
    recoveredIdentityData = undefined;
  };

  const handleRetry = () => {
    recoveryError = false;
    identityNotFoundError = false;
    manualSubmitRequired = true;
    showAll = true;
    clearTimeout(submitTimeoutId);
    submitTimeoutId = undefined;
  };

  const handleContinue = async (newName?: string) => {
    if (nonNullish(recoveredIdentityData) && !continueInProgress) {
      continueInProgress = true;
      try {
        await authenticationStore.set({
          identityNumber: recoveredIdentityData.identityNumber,
          identity: recoveredIdentityData.identity,
          authMethod: {
            recoveryPhrase: {
              identityNumber: recoveredIdentityData.identityNumber,
            },
          },
        });
        if (nonNullish(newName) && newName.length > 0) {
          try {
            await $authenticatedStore
              .actor.identity_properties_replace(
                recoveredIdentityData.identityNumber,
                {
                  name: [newName],
                },
              )
              .then(throwCanisterError);
          } catch {
            toaster.error({
              title: $t`Failed to update identity name`,
              description: $t`Your identity was restored, but the name could not be updated. Please try the recovery flow again to set a name.`,
              duration: 5000,
            });
          }
        }
        toaster.success({
          title: $t`Successfully restored access to your identity`,
          description: $t`Make sure to add a new access method so that you can sign in next time or reset your recovery phrase.`,
          duration: 5000,
        });
        await goto("/manage");
      } finally {
        resetRecoveryState();
        continueInProgress = false;
      }
    }
  };

  const handleCancel = async () => {
    showCancelDialog = false;
    resetRecoveryState();
    await goto("/login");
  };

  const handlePaste = (event: ClipboardEvent, currentIndex: number) => {
    event.preventDefault();

    // Get pasted text from clipboard
    const pastedText = event.clipboardData?.getData("text");
    if (!pastedText) {
      return;
    }

    // Uses might paste text with multiple spaces, tabs, or newlines between words.
    const pastedWords = pastedText.trim().split(/\s+/);
    // Fill inputs starting from current index.
    pastedWords.forEach((word, i) => {
      const targetIndex = currentIndex + i;
      if (targetIndex < words.length) {
        words[targetIndex].value = word.trim().toLowerCase();
        words[targetIndex].isValid = true;
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

  const toggleAll = () => {
    showAll = !showAll;
    if (!showAll) {
      words.forEach((word) => {
        word.showContent = false;
      });
    }
  };

  const handleClearAll = () => {
    clearTimeout(submitTimeoutId);
    submitTimeoutId = undefined;

    showAll = false;
    recoveryError = false;
    identityNotFoundError = false;

    words.forEach((word) => {
      word.value = "";
      word.isValid = true;
      word.showContent = false;
    });

    const firstElement = document.getElementById("recovery-phrase-0");
    if (nonNullish(firstElement)) {
      firstElement.focus();
    }
  };

  // Cleanup timeout on component unmount
  $effect(() => {
    return () => {
      clearTimeout(submitTimeoutId);
    };
  });

  const loading = $derived(recoveryInProgress || continueInProgress);
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
              <!-- "data-lpignore" Last pass ignore -->
              <!-- "data-1p-ignore" 1Password ignore -->
              <!-- "data-bwignore" Bitwarden ignore -->
              <!-- "data-form-type=other" Non-standard hint to password managers -->
              <input
                type={showAll || word.showContent || !word.isValid
                  ? "text"
                  : "password"}
                inputmode="text"
                autocorrect="off"
                autocomplete="off"
                autocapitalize="off"
                spellcheck="false"
                id={`recovery-phrase-${i}`}
                value={word.value}
                oninput={(event) => {
                  const target = event.currentTarget as HTMLInputElement;
                  word.value = target.value.toLowerCase();
                }}
                onkeydown={(e) => handleKeyDownInput(e, i)}
                onpaste={(e) => handlePaste(e, i)}
                data-lpignore="true"
                data-1p-ignore="true"
                data-bwignore="true"
                data-form-type="other"
                onfocus={() => (word.showContent = true)}
                onblur={() => {
                  validateWord(i);
                  word.showContent = false;
                }}
                disabled={loading}
                aria-invalid={!word.isValid}
                class={[
                  "peer text-text-primary h-8 w-full pr-10 pl-10 text-base",
                  "rounded-full border-none ring outline-none ring-inset focus:ring-2",
                  "disabled:bg-bg-disabled disabled:ring-border-disabled_subtle disabled:text-text-disabled",
                  word.isValid
                    ? "ring-border-secondary focus:ring-border-brand bg-transparent"
                    : "bg-bg-error-primary/30 ring-border-error focus:ring-border-error",
                ]}
              />
              {#if !word.isValid}
                <Tooltip
                  label={$t`Incorrect spelling`}
                  direction="up"
                  distance="0.5rem"
                >
                  <span
                    class="text-text-error-primary focus:ring-border-error absolute top-1/2 right-3 flex size-5 -translate-y-1/2 items-center justify-center rounded-full focus:ring-2 focus:outline-none"
                    aria-label={$t`Incorrect spelling`}
                  >
                    <InfoIcon class="size-4" />
                  </span>
                </Tooltip>
              {/if}
              <!-- Left slot -->
              <!-- Reverse order to use "peer" class to change the border color when peer is focused -->
              <span
                class={[
                  "absolute top-0 left-0 flex h-8 w-8 items-center border-r-1 px-2 text-center text-sm font-semibold",
                  "peer peer-disabled:border-border-disabled_subtle peer-focus:border-r-2",
                  word.isValid
                    ? "border-border-secondary text-text-secondary peer-focus:border-border-brand"
                    : "border-border-error text-text-error-primary peer-focus:border-border-error",
                ]}
              >
                {String(i + 1).padStart(2, "0")}
              </span>
            </label>
          {/each}
        </div>
        <div class="flex flex-row gap-2">
          <Button
            disabled={loading || !hasAnyWord}
            class="w-full"
            variant="tertiary"
            onclick={toggleAll}
          >
            {#if showAll}
              {$t`Hide all`}
            {:else}
              {$t`Show all`}
            {/if}
          </Button>
          <Button
            disabled={loading || !hasAnyWord}
            class="w-full"
            variant="tertiary"
            onclick={handleClearAll}
          >
            {$t`Clear all`}
          </Button>
        </div>
      </div>
      {#if manualSubmitRequired}
        <Button
          size="xl"
          variant="primary"
          disabled={!submitEnabled || loading}
          onclick={handleRecoverWithPhrase}
        >
          {$t`Submit`}
        </Button>
      {/if}
      <Button
        size="xl"
        variant="secondary"
        disabled={loading}
        onclick={() => (showCancelDialog = true)}
      >
        {$t`Cancel`}
      </Button>
    </div>
  </AuthPanel>
</div>

{#if recoveredIdentityData}
  {@const identityName = recoveredIdentityData.info.name[0]}
  {@const createdAt = nonNullish(recoveredIdentityData.info.created_at[0])
    ? new Date(nanosToMillis(recoveredIdentityData.info.created_at[0]))
    : undefined}
  <Dialog onClose={resetRecoveryState}>
    <SuccessfulRecovery
      identityNumber={recoveredIdentityData.identityNumber}
      {identityName}
      {createdAt}
      onContinue={handleContinue}
      onCancel={handleCancel}
    />
  </Dialog>
{/if}

{#if recoveryError}
  <Dialog>
    <RecoveryError onRetry={handleRetry} onCancel={handleCancel} />
  </Dialog>
{/if}

{#if identityNotFoundError}
  <Dialog>
    <IdentityNotFound onRetry={handleRetry} onCancel={handleCancel} />
  </Dialog>
{/if}

{#if showCancelDialog}
  <Dialog onClose={() => (showCancelDialog = false)}>
    <CancelRecovery
      onCancel={handleCancel}
      onClose={() => (showCancelDialog = false)}
    />
  </Dialog>
{/if}
