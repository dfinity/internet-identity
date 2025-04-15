<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";
  import { type AuthenticateStep } from "../../state";
  import { tick } from "svelte";

  type Props = Omit<
    Extract<AuthenticateStep, { step: "solveCaptcha" }>,
    "step"
  >;

  const { image, attempt, solve, cancel }: Props = $props();

  let inputRef: HTMLInputElement;
  let solution = $state("");
  let loading = $state(false);

  const handleSubmit = () => {
    loading = true;
    solve(solution);
  };

  $effect.pre(() => {
    if (image) {
      solution = "";
      loading = false;
      tick().then(() => {
        inputRef.focus();
      });
    }
  });
</script>

<form class="flex flex-1 flex-col gap-8">
  <div class="flex flex-col gap-4">
    <div
      class="preset-tonal relative flex items-center justify-center rounded-sm"
    >
      <img
        src={image}
        class="h-30 w-55 mix-blend-darken dark:mix-blend-lighten dark:invert"
        alt="CAPTCHA Characters"
      />
      <div
        class="dark:bg-ii-background-primary-light bg-ii-background-primary-dark absolute h-30 w-55 mix-blend-lighten dark:mix-blend-darken"
      ></div>
    </div>
    <p>
      {attempt > 0
        ? "Incorrect solution, try again"
        : "Type the characters you see"}
    </p>
    <input
      bind:this={inputRef}
      bind:value={solution}
      disabled={loading}
      class="input px-4 py-2"
      type="text"
      autocapitalize="none"
      spellcheck="false"
    />
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-4">
    <Button
      onclick={handleSubmit}
      type="submit"
      disabled={solution.length === 0 || loading}
      variant="primary">{loading ? "Loading..." : "Submit"}</Button
    >
    <Button onclick={cancel} disabled={loading} variant="secondary"
      >Cancel</Button
    >
  </div>
</form>
