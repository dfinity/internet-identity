<script lang="ts">
  import { fly } from "svelte/transition";
  import { type State } from "../state";
  import { onMount } from "svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";

  type Props = Extract<State, { state: "createPasskey" }>;

  const { create, cancel }: Props = $props();

  let inputRef: HTMLInputElement;
  let name = $state("");

  const handleSubmit = () => {
    create(name);
  };

  onMount(() => {
    inputRef.focus();
    authenticationV2Funnel.trigger(AuthenticationV2Events.EnterNameScreen);
    // Make sure mobile keyboard does not overlap the input
    setTimeout(() => inputRef.scrollIntoView(), 100);
  });
</script>

<form class="flex flex-1 flex-col gap-8">
  <div class="flex flex-col gap-6" in:fly={{ duration: 200, x: 10 }}>
    <p class="opacity-80">
      You're about to create a passkey, a secure way to sign in. To help you
      recognize it later, give your passkey a name that reflects your identity.
    </p>
    <label class="label">
      <span class="label-text">Name</span>
      <input
        bind:this={inputRef}
        bind:value={name}
        class="input px-4 py-2"
        type="text"
      />
    </label>
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-4">
    <button
      onclick={handleSubmit}
      type="submit"
      disabled={name.length === 0}
      class="btn preset-filled py-2">{"Create Passkey"}</button
    >
    <button onclick={cancel} class="btn preset-outlined py-2">Go back</button>
  </div>
</form>
