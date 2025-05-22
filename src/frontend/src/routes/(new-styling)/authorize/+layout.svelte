<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { onMount } from "svelte";
  import CenterLayout from "$lib/components/layout/CenterLayout.svelte";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { nonNullish } from "@dfinity/utils";
  import { navigating } from "$app/state";
  import { goto } from "$app/navigation";

  const { children }: LayoutProps = $props();

  const { context, status } = $derived($authorizationStore);
  const lastUsedIdentityAvailable = $derived(
    nonNullish(context) &&
      nonNullish(
        Object.values($lastUsedIdentitiesStore).sort(
          (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
        )[0],
      ),
  );

  $effect.pre(() => {
    if (lastUsedIdentityAvailable) {
      // By redirecting within `$effect.pre` we redirect before the page update
      // is rendered while making sure that we don't render the previous page
      // by checking `navigating.to.url.pathname !== "/authorize/continue"`.
      goto("/authorize/continue", { replaceState: true });
    }
  });

  onMount(() => {
    authorizationStore.init();
  });
</script>

<CenterLayout data-page="new-authorize-view">
  {#if status === "authenticating" && navigating.to?.url.pathname !== "/authorize/continue"}
    <AuthPanel>
      {@render children()}
    </AuthPanel>
  {:else if status === "authorizing"}
    <div class="flex flex-col items-center justify-center gap-4">
      <ProgressRing class="text-fg-primary size-14" />
      <p class="text-text-secondary text-lg">Redirecting to the app</p>
    </div>
  {:else if status === "orphan" || status === "closed" || status === "invalid" || status === "failure"}
    <div>Error</div>
  {:else if status === "success"}
    <div>Success</div>
  {/if}
</CenterLayout>
