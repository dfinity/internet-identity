<script lang="ts">
  import { onMount } from "svelte";
  import { type State } from "../state";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";

  type Props = Extract<State, { state: "continueAs" }>;

  const { number, name, continue: continueFn, useAnother }: Props = $props();

  onMount(() => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsScreen);
  });

  const handleUseAnother = () => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.UseAnother);
    useAnother();
  };
</script>

<div class="flex flex-col items-stretch gap-4">
  <button
    onclick={continueFn}
    class="btn preset-filled py-4 pr-4 pl-12 text-left"
  >
    <span class="flex-1 text-center">
      Continue as <span class="font-medium">{name ?? number}</span>
    </span>
    <span class="min-w-8 text-center text-lg">→</span>
  </button>
  <button
    onclick={handleUseAnother}
    class="btn preset-tonal px-6 py-2 text-left">Use another identity</button
  >
</div>
