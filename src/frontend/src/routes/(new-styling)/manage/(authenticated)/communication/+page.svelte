<script lang="ts">
  /**
   * Communication page — Phase 1's home for `Anchor.verified_emails`.
   * Sibling of the Recovery page: this surface manages the addresses
   * dapps can use to reach the user, while Recovery manages methods
   * for regaining access to the anchor itself.
   *
   * Page chrome mirrors the access page (bare `<header>` + content;
   * no outer card wrapper — individual rows carry their own border).
   * Phase 1.5 widens this page into the unified "Reach" experience
   * (verified rows joined with unverified OIDC/SSO emails + per-row
   * "Verify" CTA); for Phase 1 the page is just the verified-emails
   * panel.
   */
  import { VerifiedEmailsPanel } from "$lib/components/settings";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  /**
   * Per-anchor cap mirrors `MAX_VERIFIED_EMAILS_PER_ANCHOR` on the
   * canister; inlined here because the candid API doesn't (yet)
   * expose it. Keep in sync with
   * `src/internet_identity/src/email_inbound/mod.rs`.
   */
  const VERIFIED_EMAILS_CAPACITY = 5;
  let verifiedEmails = $derived(data.identityInfo.verified_emails[0] ?? []);
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Communication`}</h1>
  <p class="text-text-tertiary text-base">
    <Trans>How apps can reach you when you sign in.</Trans>
  </p>
</header>

<div class="mt-10 max-w-3xl">
  <VerifiedEmailsPanel {verifiedEmails} capacity={VERIFIED_EMAILS_CAPACITY} />
</div>
