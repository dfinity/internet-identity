<script lang="ts">
  import { ExternalLinkIcon, LockIcon } from "@lucide/svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
</script>

<!--
  Shown when the user has CLI access disabled for this identity on this
  device and a dapp-mode CLI sign-in is requested. The toggle is purely
  client-side (localStorage), so we know about the gate before issuing
  any backend call — the user lands here immediately on page load.
-->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-100">
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-5 self-start">
      <LockIcon class="size-6" />
    </FeaturedIcon>

    <h1 class="text-text-primary text-2xl font-medium">
      {$t`CLI access not enabled`}
    </h1>
    <p class="text-text-tertiary mt-4 text-base text-pretty">
      <Trans>
        For security, Internet Identity blocks command-line tools from signing
        in to apps using your identity.
      </Trans>
    </p>
    <p class="text-text-tertiary mt-4 text-base text-pretty">
      <Trans>Enable CLI access for this device, then try again.</Trans>
    </p>

    <a
      href="/manage/settings"
      target="_blank"
      rel="noopener noreferrer"
      class="btn btn-secondary btn-xl mt-8 w-full"
    >
      <ExternalLinkIcon class="size-5" />
      <span>{$t`Manage your identity`}</span>
    </a>
  </AuthPanel>
</div>
