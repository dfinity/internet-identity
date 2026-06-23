<script lang="ts">
  /**
   * Narrow "Verified emails" settings panel — Phase 1 of the verified-
   * email feature. Lists `verified_emails` entries from the
   * authenticated `IdentityInfo`, lets the user add a new one via
   * {@link VerifiedEmailWizard}, and lets the user remove an existing
   * one. Phase 1.5 widens this into the unified "Reach" page (verified
   * via IdP _or_ via this flow + unverified IdP rows); Phase 2 wires
   * `verified_emails` into the attribute-source pipeline so dapps can
   * actually request these addresses.
   *
   * Recovery emails are intentionally NOT shown here — they live on
   * the existing recovery card and remain a distinct concept.
   */

  import { MailIcon, PlusIcon, Trash2Icon } from "@lucide/svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { anonymousActor } from "$lib/globals";
  import { invalidateAll } from "$app/navigation";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { VerifiedEmailWizard } from "$lib/components/wizards/verifiedEmail";
  import RemoveVerifiedEmail from "./RemoveVerifiedEmail.svelte";
  import { Trans } from "$lib/components/locale";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import { toaster } from "$lib/components/utils/toaster";
  import { handleError } from "$lib/components/utils/error";
  import { nanosToMillis } from "$lib/utils/time";
  import { throwCanisterError } from "$lib/utils/utils";
  import type {
    EmailChallengeDnsInput,
    EmailChallengeSubmitDkimLeafArg,
    VerifiedEmail,
  } from "$lib/generated/internet_identity_types";

  interface Props {
    /** Current `verified_emails` list — comes from `IdentityInfo` via
     *  the (authenticated) layout. The panel renders this list as-is;
     *  add/remove call `invalidateAll()` so the layout re-fetches. */
    verifiedEmails: VerifiedEmail[];
    /** Per-anchor cap, surfaced from the canister via
     *  `MAX_VERIFIED_EMAILS_PER_ANCHOR`. Wired in as a prop so the
     *  panel doesn't hard-code it; bumping the constant is a backend
     *  change that should propagate without a frontend edit. */
    capacity: number;
  }

  const { verifiedEmails, capacity }: Props = $props();

  let showAddWizard = $state(false);
  let removingAddress = $state<string | undefined>(undefined);

  const isFull = $derived(verifiedEmails.length >= capacity);

  // --- Canister wrappers (same as recovery wizard) -------------------

  /** Authenticated wrapper around `verified_email_prepare_add`. */
  const prepareAddVerifiedEmail = (input: EmailChallengeDnsInput) =>
    $authenticatedStore.actor
      .verified_email_prepare_add($authenticatedStore.identityNumber, input)
      .then(throwCanisterError);

  /** Anonymous wrappers — status / diagnostics / submit / DoH are
   *  shared with the recovery flow (keyed by nonce, not by purpose). */
  const statusEmailRecovery = (nonce: string) =>
    anonymousActor.email_challenge_status(nonce);

  const diagnosticsEmailRecovery = (nonce: string) =>
    anonymousActor.email_challenge_diagnostics(nonce);

  const submitEmailDkimLeaf = async (
    arg: EmailChallengeSubmitDkimLeafArg,
  ): Promise<void> => {
    await throwCanisterError(
      await anonymousActor.email_challenge_submit_dkim_leaf(arg),
    );
  };

  const resolveEmailViaDoh = async (nonce: string): Promise<void> => {
    await throwCanisterError(
      await anonymousActor.email_challenge_resolve_via_doh({ nonce }),
    );
  };

  // --- Handlers ------------------------------------------------------

  const handleAddSuccess = (address: string) => {
    showAddWizard = false;
    void invalidateAll();
    toaster.success({
      title: $t`Email verified`,
      description: $t`${address} is now available for dapps to request.`,
    });
  };

  const handleAddClosed = () => {
    showAddWizard = false;
    void invalidateAll();
  };

  const handleRemove = async (address: string) => {
    const result = await $authenticatedStore.actor.verified_email_remove(
      $authenticatedStore.identityNumber,
      address,
    );
    if ("Err" in result) {
      handleError(new Error(JSON.stringify(result.Err)));
      return;
    }
    removingAddress = undefined;
    void invalidateAll();
    toaster.success({
      title: $t`Verified email removed`,
      description: $t`${address} has been removed from your verified emails.`,
    });
  };
</script>

<section class="flex flex-col gap-4" aria-labelledby="email-addresses-heading">
  <div class="flex flex-row items-start justify-between gap-4">
    <div class="flex flex-col gap-1">
      <div class="flex flex-row items-center gap-3">
        <h2
          id="email-addresses-heading"
          class="text-text-primary text-lg font-semibold"
        >{$t`Email addresses`}</h2>
        <span
          class="border-border-tertiary text-text-secondary inline-flex items-center rounded-full border px-2 py-0.5 text-xs font-medium"
          aria-label={$t`${verifiedEmails.length} of ${capacity} verified emails`}
        >
          {verifiedEmails.length}
        </span>
      </div>
      <p class="text-text-tertiary text-sm">
        <Trans>Only verified emails can be used to reach you.</Trans>
      </p>
    </div>
    <button
      class="btn btn-primary btn-sm shrink-0"
      onclick={() => (showAddWizard = true)}
      disabled={isFull}
      aria-label={$t`Add an email`}
    >
      <PlusIcon class="size-4" aria-hidden="true" />
      <span>{$t`Add an email`}</span>
    </button>
  </div>

  {#if verifiedEmails.length === 0}
    <div class="flex flex-col items-center gap-3 px-6 py-7 text-center">
      <span
        class="bg-bg-secondary border-border-secondary text-fg-primary flex size-10 items-center justify-center rounded-full border"
      >
        <MailIcon class="size-5" aria-hidden="true" />
      </span>
      <p class="text-text-tertiary max-w-xs text-sm">
        <Trans>
          No emails yet. Verify one so apps can use it to reach you.
        </Trans>
      </p>
    </div>
  {:else}
    <ul class="flex flex-col gap-2">
      {#each verifiedEmails as entry (entry.address)}
        {@const verifiedAt = new Date(nanosToMillis(entry.verified_at))}
        <li
          class="bg-bg-secondary border-border-secondary flex flex-row items-center gap-3 rounded-xl border px-4 py-3"
        >
          <div class="flex flex-1 flex-col gap-1 overflow-hidden">
            <div class="flex flex-row items-center gap-2.5">
              <span
                class="text-text-primary min-w-0 truncate text-sm font-semibold"
              >
                {entry.address}
              </span>
              <span
                class="border-color-success-600 bg-bg-success-primary text-text-success-primary inline-flex shrink-0 items-center gap-1.5 rounded-full border px-2 py-0.5 text-xs font-medium"
              >
                <span
                  class="bg-fg-success-primary size-1.5 rounded-full"
                  aria-hidden="true"
                ></span>
                {$t`Verified`}
              </span>
            </div>
            <Tooltip
              label={$formatDate(verifiedAt, {
                timeStyle: "short",
                dateStyle: "medium",
              })}
              direction="up"
              align="start"
            >
              <span class="text-text-tertiary text-sm">
                {$t`Verified`}
                {$formatRelative(verifiedAt, { style: "long" })}
              </span>
            </Tooltip>
          </div>
          <button
            class="btn btn-tertiary btn-sm btn-icon shrink-0"
            onclick={() => {
              removingAddress = entry.address;
            }}
            aria-label={$t`Remove ${entry.address}`}
          >
            <Trash2Icon class="size-4" aria-hidden="true" />
          </button>
        </li>
      {/each}
    </ul>
  {/if}

  {#if isFull}
    <p class="text-text-tertiary text-xs">
      <Trans>
        You've reached the limit of {capacity} verified emails. Remove one before
        adding another.
      </Trans>
    </p>
  {/if}
</section>

{#if showAddWizard}
  <Dialog onClose={handleAddClosed} closeOnOutsideClick={false}>
    <VerifiedEmailWizard
      prepare={prepareAddVerifiedEmail}
      status={statusEmailRecovery}
      diagnostics={diagnosticsEmailRecovery}
      submitDkimLeaf={submitEmailDkimLeaf}
      resolveViaDoh={resolveEmailViaDoh}
      onSuccess={handleAddSuccess}
    />
  </Dialog>
{/if}

{#if removingAddress !== undefined}
  {@const target = removingAddress}
  <Dialog onClose={() => (removingAddress = undefined)}>
    <RemoveVerifiedEmail
      address={target}
      onRemove={() => handleRemove(target)}
      onCancel={() => (removingAddress = undefined)}
    />
  </Dialog>
{/if}
