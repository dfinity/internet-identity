<script lang="ts">
  /**
   * Communication page — Phase 1's home for `Anchor.verified_emails`.
   * Sibling of the Recovery page: this surface manages the addresses
   * dapps can use to reach the user, while Recovery manages methods
   * for regaining access to the anchor itself. The two used to share
   * the `/manage/recovery` route while verified-emails was a narrow
   * panel; per the production design they're separate top-level
   * destinations now.
   *
   * Phase 1.5 will widen this page into the unified "Reach"
   * experience (verified rows joined with unverified OIDC/SSO emails
   * + per-row "Verify" CTA); for Phase 1 the page is just the
   * verified-emails panel.
   */
  import { VerifiedEmailsPanel } from "$lib/components/settings";
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

<div class="grid max-w-5xl grid-cols-1 gap-5">
  <VerifiedEmailsPanel {verifiedEmails} capacity={VERIFIED_EMAILS_CAPACITY} />
</div>
