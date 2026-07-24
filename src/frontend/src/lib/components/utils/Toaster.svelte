<script lang="ts">
  import { normalizeProps, useMachine } from "@zag-js/svelte";
  import * as toast from "@zag-js/toast";
  import Toast from "$lib/components/utils/Toast.svelte";
  import { toaster } from "$lib/components/utils/toaster";

  const id = $props.id();
  const service = useMachine(toast.group.machine, {
    id,
    store: toaster,
  });

  const api = $derived(toast.group.connect(service, normalizeProps));
</script>

<!-- The toast group is positioned fixed at the top-end of the viewport.
     On mobile (full-width toast) the group's box can overlap visible
     page content underneath; without `pointer-events: none` here it
     intercepts clicks on whatever sits under it. Each Toast card
     re-enables pointer-events on itself so the Close button + action
     buttons still work. -->
<div {...api.getGroupProps()} class="pointer-events-none">
  {#each api.getToasts() as toast, index (toast.id)}
    <Toast {toast} {index} parent={service} />
  {/each}
</div>
