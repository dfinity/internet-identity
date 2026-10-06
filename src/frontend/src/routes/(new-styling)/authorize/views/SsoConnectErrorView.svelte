<script lang="ts">
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { t } from "$lib/stores/locale.store";
  import { ssoErrorMessage } from "$lib/components/wizards/auth";

  interface Props {
    error: unknown;
    domain: string;
  }

  const { error, domain }: Props = $props();
</script>

<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-100">
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Couldn't connect to your organization`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base font-medium text-pretty">
      {ssoErrorMessage(error, domain)}
    </p>
    <button class="btn btn-secondary" onclick={() => window.close()}>
      <RotateCcwIcon class="size-4" />
      <span>{$t`Return to app`}</span>
    </button>
  </AuthPanel>
</div>
