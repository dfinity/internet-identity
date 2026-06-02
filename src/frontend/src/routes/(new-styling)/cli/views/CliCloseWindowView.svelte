<script lang="ts">
  import { CheckIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { t } from "$lib/stores/locale.store";
</script>

<!--
  Centered, card-less "you're done" page. Once the loopback callback has
  accepted the delegation there's nothing actionable here; the user just
  closes the tab and returns to the terminal.
-->
<div class="flex flex-col items-center gap-5 px-6 text-center">
  <FeaturedIcon size="lg" variant="success">
    <CheckIcon class="size-6" strokeWidth={2.4} />
  </FeaturedIcon>
  <h1 class="text-text-primary text-2xl font-medium">
    {$t`You're signed in`}
  </h1>
  <p class="text-text-tertiary text-base">
    {$t`You can close this window.`}
  </p>
</div>
