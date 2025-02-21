<svelte:options customElement="custom-input" />

<script lang="ts">

  interface Props {
    value: string;
    color: string;
    submit: (value: string) => void;
  }

  let { value, color, submit }: Props = $props();

  $effect(() => {
    $host().dispatchEvent(new CustomEvent("value", { detail: value }));
  });
</script>

<input bind:value={value} style="--color:{color}" />
<button onclick={() => submit(value)}>Submit</button>

<style>
    input {
        color: var(--color);
    }
</style>