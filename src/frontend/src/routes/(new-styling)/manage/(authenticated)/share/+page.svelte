<script lang="ts">
  import { VerifiedEmailsPanel } from "$lib/components/settings";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  // Mirrors MAX_VERIFIED_EMAILS_PER_ANCHOR on the canister; not (yet)
  // exposed via candid. Keep in sync with email_inbound/mod.rs.
  const VERIFIED_EMAILS_CAPACITY = 5;
  let verifiedEmails = $derived(data.identityInfo.verified_emails[0] ?? []);
  let recoveryAddresses = $derived(
    (data.identityInfo.email_recovery[0] ?? []).map((c) => c.address),
  );
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Shareable info`}</h1>
  <p class="text-text-tertiary text-base">
    <Trans>
      Info you can choose to share when an app requests it. Without your
      consent, your info is always kept secret.
    </Trans>
  </p>
</header>

<div class="mt-10 max-w-3xl">
  <VerifiedEmailsPanel
    {verifiedEmails}
    {recoveryAddresses}
    capacity={VERIFIED_EMAILS_CAPACITY}
  />
</div>
