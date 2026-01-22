<script lang="ts">
  import { goto } from "$app/navigation";
  import type { PageProps } from "./$types";
  import { onMount } from "svelte";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { requestAuthFromOpener } from "./utils";

  const { data }: PageProps = $props();

  // Receives authentication state from window opener
  // and then redirects to next from search params.
  onMount(async () => {
    const authenticated = await requestAuthFromOpener();
    await authenticationStore.set(authenticated);
    await goto(data.next ?? "/manage", { replaceState: true });
  });
</script>

<div class="bg-bg-primary_alt flex min-h-[100dvh] flex-col">
  <ProgressRing class="text-fg-tertiary m-auto size-16" />
</div>
