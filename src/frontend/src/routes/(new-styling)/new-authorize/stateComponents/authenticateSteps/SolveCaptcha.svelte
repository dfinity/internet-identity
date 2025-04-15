<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";
  import { type AuthenticateStep } from "../../state";

  type Props = Omit<
    Extract<AuthenticateStep, { step: "solveCaptcha" }>,
    "step"
  >;

  const { image, attempt, solve, cancel }: Props = $props();

  let solution = $state("");

  $effect(() => {
    if (image) {
      solution = "";
    }
  });
</script>

<form class="flex flex-1 flex-col gap-8">
  <div class="flex flex-col gap-4">
    <div class="preset-tonal relative flex items-center justify-center">
      <img
        src={image}
        class="h-30 w-55 mix-blend-darken dark:mix-blend-lighten dark:invert"
        alt="CAPTCHA Characters"
      />
      <div
        class="dark:bg-ii-background-primary-light bg-ii-background-primary-dark absolute h-30 w-55 mix-blend-lighten dark:mix-blend-darken"
      ></div>
    </div>
    {#if attempt > 0}
      <p class="p">Incorrect solution, try again</p>
    {:else}
      <p class="p">Type the characters you see</p>
    {/if}
    <input
      bind:value={solution}
      class="input px-4 py-2"
      type="text"
      autofocus
      autocapitalize="none"
      spellcheck="false"
    />
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-4">
    <Button
      onclick={() => solve(solution)}
      type="submit"
      disabled={solution.length === 0}
      variant="primary">Submit</Button
    >
    <Button onclick={cancel} variant="secondary">Cancel</Button>
  </div>
</form>
