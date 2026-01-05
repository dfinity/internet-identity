<script lang="ts">
  import { goto } from "$app/navigation";
  import type { PageProps } from "./$types";
  import { onMount } from "svelte";

  const { data }: PageProps = $props();

  onMount(() => {
    goto("/", { state: { login: data.next }, replaceState: true });
  });
</script>
