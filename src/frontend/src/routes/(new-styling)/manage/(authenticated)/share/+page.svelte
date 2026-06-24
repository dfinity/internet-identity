<script lang="ts">
  /**
   * "Shareable info" page — Phase 1's home for
   * `Anchor.verified_emails`. Sibling of the Recovery page: this
   * surface manages info the user can share with apps, while Recovery
   * manages methods for regaining access to the anchor itself.
   *
   * Page chrome mirrors the access page (bare `<header>` + content;
   * no outer card wrapper — individual rows carry their own border).
   * Phase 1.5 widens this surface with unverified OIDC/SSO emails +
   * a per-row "Verify" CTA; for Phase 1 it's just the verified-emails
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
  // Threaded into the verify wizard for the cross-bucket overlap
  // heads-up: when a user types an address that's already their
  // recovery email, the wizard warns that the two buckets are
  // independent (verifying here is a second DKIM round-trip).
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
