<script lang="ts" generics="T extends unknown[]">
  import { onMount } from "svelte";
  import { render as litRender } from "lit-html";
  import type { TemplateElement } from "$src/utils/lit-html";

  interface Props {
    render: (...args: T) => TemplateElement;
    args: T;
  }

  const { render, args }: Props = $props();
  let ref: HTMLDivElement;

  onMount(async () => {
    const template = render(...args);
    litRender(template, ref);
  });
</script>
<link rel="stylesheet" href="./src/frontend/src/styles/main.css" />

<div bind:this={ref}></div>