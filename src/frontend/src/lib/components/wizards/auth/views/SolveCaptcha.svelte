<script lang="ts">
  import { tick } from "svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    image: string;
    attempt: number;
    solve: (solution: string) => void;
  }

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

<form class="flex flex-1 flex-col">
  <div class="mt-auto mb-6 flex flex-col gap-2">
    <h1 class="text-text-primary text-2xl font-medium">Verify you are human</h1>
    <p class="text-text-tertiary text-sm">
      Type the characters exactly as shown below
    </p>
  </div>
  <div
    class="border-border-secondary bg-bg-primary relative mb-3 flex items-center justify-center rounded-lg border"
  >
    <img
      src={image}
      class="h-30 w-55 mix-blend-darken dark:mix-blend-lighten dark:invert"
      alt="CAPTCHA Characters"
    />
    <div
      class={"bg-text-primary absolute inset-0 mix-blend-lighten dark:mix-blend-darken"}
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
