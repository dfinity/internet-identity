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

  import {
    EllipsisVerticalIcon,
    MailCheckIcon,
    Trash2Icon,
  } from "@lucide/svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { anonymousActor } from "$lib/globals";
  import { invalidateAll } from "$app/navigation";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Select from "$lib/components/ui/Select.svelte";
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

<section
  class="bg-bg-primary border-border-secondary flex flex-col rounded-2xl border p-6 not-dark:shadow-sm"
>
  <div class="mb-3 flex flex-row items-center">
    <MailCheckIcon class="text-fg-tertiary size-6" />
    <span
      class="text-text-tertiary ms-auto text-xs"
      aria-label={$t`${verifiedEmails.length} of ${capacity} verified emails`}
    >
      {verifiedEmails.length}/{capacity}
    </span>
  </div>
  <h2 class="text-text-primary mb-1 text-base font-semibold">
    {$t`Verified emails`}
  </h2>
  <div class="text-text-tertiary mb-5 text-sm">
    <Trans>
      Apps can request one of these to reach you. Never shared without your
      consent.
    </Trans>
  </div>

  {#if verifiedEmails.length === 0}
    <div
      class="border-border-tertiary text-text-tertiary mb-5 flex flex-col gap-2 rounded-xl border border-dashed p-4 text-sm"
    >
      <Trans>
        Verify an email so apps can reach you when you sign in — Internet
        Identity never shares it without asking.
      </Trans>
    </div>
  {:else}
    <ul class="border-border-tertiary mb-5 flex flex-col border-y">
      {#each verifiedEmails as entry (entry.address)}
        {@const verifiedAt = new Date(nanosToMillis(entry.verified_at))}
        <li
          class="border-border-tertiary flex flex-row items-center gap-3 py-3 not-last:border-b"
        >
          <div class="flex flex-1 flex-col gap-0.5 overflow-hidden">
            <div class="text-text-primary truncate text-sm font-medium">
              {entry.address}
            </div>
            <Tooltip
              label={$formatDate(verifiedAt, {
                timeStyle: "short",
                dateStyle: "medium",
              })}
              direction="up"
              align="start"
            >
              <span class="text-text-tertiary text-xs">
                {$t`Verified`}
                {$formatRelative(verifiedAt, { style: "long" })}
              </span>
            </Tooltip>
          </div>
          <Select
            options={[
              {
                label: $t`Remove`,
                icon: Trash2Icon,
                onClick: () => {
                  removingAddress = entry.address;
                },
              },
            ]}
            align="end"
          >
            <button
              class="btn btn-tertiary btn-sm btn-icon"
              aria-label={$t`More options for ${entry.address}`}
            >
              <EllipsisVerticalIcon class="size-5" />
            </button>
          </Select>
        </li>
      {/each}
    </ul>
  {/if}

  <button
    class="btn btn-primary btn-sm self-start"
    onclick={() => (showAddWizard = true)}
    disabled={isFull}
    aria-label={$t`Verify an email`}
  >
    {$t`Verify an email`}
  </button>
  {#if isFull}
    <p class="text-text-tertiary mt-3 text-xs">
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
