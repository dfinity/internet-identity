<script lang="ts">
  import { CheckIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { t } from "$lib/stores/locale.store";
  import { onMount } from "svelte";

  interface Props {
    /** Whether the tab is being handed to the trusted server (to redeem the
     *  registration delegation and finish the flow). When false this is the
     *  resting screen and there's nothing to redirect to. */
    redirecting: boolean;
  }

  const { redirecting }: Props = $props();

  // A successful hand-off replaces the document (and unmounts this view) within
  // a moment. If we're still mounted after a few seconds the navigation never
  // took — a server endpoint that doesn't replace the document, or a bfcache
  // back-navigation to this restored page — so reveal a way out rather than
  // leaving the user on a "Redirecting…" message that never ends.
  let redirectStalled = $state(false);
  onMount(() => {
    if (!redirecting) {
      return;
    }
    const timer = setTimeout(() => (redirectStalled = true), 5000);
    return () => clearTimeout(timer);
  });
</script>

<!--
  Centered, card-less success page shown once the registration delegation has
  been minted. The tab is then handed to the server's declared callback —
  where the server redeems the delegation and finishes the flow — so we say a
  redirect is underway (with a fallback hint if it stalls); when there's nothing
  to redirect to this is the resting screen, so we just confirm the sign-in.
-->
<div class="flex flex-col items-center gap-5 px-6 text-center">
  <FeaturedIcon size="lg" variant="success">
    <CheckIcon class="size-6" strokeWidth={2.4} />
  </FeaturedIcon>
  <h1 class="text-text-primary text-2xl font-medium">
    {$t`You're signed in`}
  </h1>
  <div class="flex flex-col items-center gap-1">
    <p class="text-text-tertiary text-base">
      {#if redirecting}
        {$t`Redirecting back to the agent…`}
      {:else}
        {$t`You can return to your agent.`}
      {/if}
    </p>
    {#if redirecting && redirectStalled}
      <p class="text-text-tertiary text-sm">
        {$t`If nothing happens, you can return to your agent.`}
      </p>
    {/if}
  </div>
</div>
