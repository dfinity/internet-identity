<script lang="ts">
  import { type State } from "../state";
  import { tick } from "svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  type Props = Extract<State, { state: "solveCaptcha" }>;

  const { image, attempt, solve }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let solution = $state("");
  let loading = $state(false);
  let error = $state(false);

  const handleSubmit = () => {
    loading = true;
    solve(solution);
  };

  $effect(() => {
    if (solution.length > 0) {
      error = false;
    }
  });

  $effect.pre(() => {
    if (image) {
      solution = "";
      loading = false;
      if (attempt > 0) {
        error = true;
      }
      tick().then(() => {
        inputRef?.focus();
      });
    }
  });
</script>

<form class="flex flex-col pt-1">
  <div class="mb-6 flex flex-col gap-2">
    <h1 class="text-gray-light-900 dark:text-gray-dark-25 text-2xl font-medium">
      Verify you are human
    </h1>
    <p class="text-gray-light-700 dark:text-gray-dark-50 text-sm">
      Type the characters exactly as shown below
    </p>
  </div>
  <div
    class={[
      "relative mb-3 flex items-center justify-center rounded-lg border",
      "border-gray-light-300 bg-white",
      "dark:border-gray-light-600 dark:bg-gray-dark-950",
    ]}
  >
    <img
      src={image}
      class="h-30 w-55 mix-blend-darken dark:mix-blend-lighten dark:invert"
      alt="CAPTCHA Characters"
    />
    <div
      class={[
        "absolute h-30 w-55",
        "bg-gray-light-700 mix-blend-lighten",
        "dark:bg-gray-light-300 dark:mix-blend-darken",
      ]}
    ></div>
  </div>
  <Input
    bind:element={inputRef}
    bind:value={solution}
    disabled={loading}
    placeholder="Enter characters"
    type="text"
    autocapitalize="none"
    autocomplete="off"
    spellcheck="false"
    class="mb-6"
    hint="Characters are case-sensitive"
    error={error ? "Incorrect. Try again with the new code." : undefined}
  />
  <Button
    onclick={handleSubmit}
    type="submit"
    disabled={solution.length === 0 || loading}
    size="lg"
  >
    {#if loading}
      <ProgressRing />
      <span>Verifying...</span>
    {:else}
      <span>Verify</span>
    {/if}
  </Button>
</form>
