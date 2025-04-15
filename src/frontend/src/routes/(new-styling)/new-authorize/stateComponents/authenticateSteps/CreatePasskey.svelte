<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";
  import { fly } from "svelte/transition";
  import { type AuthenticateStep } from "../../state";
  import { onMount } from "svelte";

  type Props = Omit<
    Extract<AuthenticateStep, { step: "createPasskey" }>,
    "step"
  >;

  const { create, back }: Props = $props();

  let inputRef: HTMLInputElement;
  let name = $state("");
  let loading = $state(false);

  const handleSubmit = () => {
    loading = true;
    create(name);
  };

  onMount(() => {
    inputRef.focus();
  });
</script>

<form class="flex flex-1 flex-col gap-8">
  <div class="flex flex-col gap-4" in:fly={{ duration: 200, x: 10 }}>
    <p>
      Explicabo corrupti temporibus consequuntur quae accusamus eligendi eius,
      ducimus iste iure.
    </p>
    <label class="label">
      <span class="label-text">Name</span>
      <input
        bind:this={inputRef}
        bind:value={name}
        disabled={loading}
        class="input px-4 py-2"
        type="text"
      />
    </label>
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-4">
    <Button
      onclick={handleSubmit}
      type="submit"
      disabled={name.length === 0 || loading}
      variant="primary">{loading ? "Loading..." : "Create Passkey"}</Button
    >
    <Button onclick={back} disabled={loading} variant="secondary">Cancel</Button
    >
  </div>
</form>
