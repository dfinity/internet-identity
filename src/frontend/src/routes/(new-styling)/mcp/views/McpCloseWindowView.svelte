<script lang="ts">
  import { CheckIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Whether the tab is being redirected back to the agent — true when the
     *  trusted server returned a `finish_url`. When false this is the resting
     *  screen and there's nothing to redirect to. */
    redirecting: boolean;
  }

  const { redirecting }: Props = $props();
</script>

<!--
  Centered, card-less success page shown once the MCP server's session key has
  been registered with the backend. When the trusted server returned a
  `finish_url` the tab is being sent back to the agent to finish the flow, so we
  say a redirect is underway; otherwise there's nothing to redirect to and this
  is the resting screen, so we just confirm the sign-in.
-->
<div class="flex flex-col items-center gap-5 px-6 text-center">
  <FeaturedIcon size="lg" variant="success">
    <CheckIcon class="size-6" strokeWidth={2.4} />
  </FeaturedIcon>
  <h1 class="text-text-primary text-2xl font-medium">
    {$t`You're signed in`}
  </h1>
  <p class="text-text-tertiary text-base">
    {#if redirecting}
      {$t`Redirecting back to the agent…`}
    {:else}
      {$t`You can return to your agent.`}
    {/if}
  </p>
</div>
