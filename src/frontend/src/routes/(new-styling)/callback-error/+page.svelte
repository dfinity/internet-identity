<script lang="ts">
  import { TriangleAlertIcon } from "@lucide/svelte";
  import { page } from "$app/state";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";

  // The frontend canister redirects here when it can't translate the IdP's
  // form_post callback (a malformed or misconfigured provider response). The
  // short machine reason rides along as a query param — shown as secondary
  // detail to help debug a provider integration, not as the primary message.
  const reason = $derived(page.url.searchParams.get("reason"));
</script>

<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-5 self-start">
      <TriangleAlertIcon class="size-6" />
    </FeaturedIcon>

    <h1 class="text-text-primary text-2xl font-medium">
      {$t`Sign-in could not be completed`}
    </h1>
    <p class="text-text-tertiary mt-4 text-base text-pretty">
      <Trans>
        Something went wrong while signing in with your provider. Close this
        window and try again.
      </Trans>
    </p>
    {#if reason !== null}
      <p class="text-text-quaternary mt-4 text-sm">
        {reason}
      </p>
    {/if}
  </AuthPanel>
</div>
