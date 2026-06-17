<script lang="ts">
  import { CircleAlertIcon } from "@lucide/svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
</script>

<!-- Terminal screen for an MCP-reported failure (status=error redirect-back). -->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Something went wrong`}
    </h1>
    <p class="text-text-tertiary mb-2 text-base text-pretty">
      <Trans>The MCP client couldn't finish connecting.</Trans>
    </p>
    <p class="text-text-tertiary text-base text-pretty">
      <Trans>
        Return to your MCP client and try again. You can close this window.
      </Trans>
    </p>
  </AuthPanel>
</div>
