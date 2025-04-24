<script lang="ts">
  import { type State } from "../state";
  import { tick } from "svelte";
  import Button from "$lib/components/UI/Button.svelte";

  type Props = Extract<State, { state: "solveCaptcha" }>;

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
    <button
      onclick={handleSubmit}
      type="submit"
      disabled={solution.length === 0 || loading}
      class="btn preset-filled py-2">{loading ? "Loading..." : "Submit"}</button
    >
    <button onclick={cancel} disabled={loading} class="btn preset-outlined py-2"
      >Cancel</button
    >
  </div>
</form>
