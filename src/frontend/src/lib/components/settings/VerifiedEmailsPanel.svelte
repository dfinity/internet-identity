<script lang="ts">
  import { MailIcon, PlusIcon, Trash2Icon } from "@lucide/svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { anonymousActor } from "$lib/globals";
  import { invalidateAll } from "$app/navigation";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { VerifiedEmailWizard } from "$lib/components/wizards/verifiedEmail";
  import RemoveVerifiedEmail from "./RemoveVerifiedEmail.svelte";
  import { Trans } from "$lib/components/locale";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import { toaster } from "$lib/components/utils/toaster";
  import { handleError } from "$lib/components/utils/error";
  import { nanosToMillis } from "$lib/utils/time";
  import { throwCanisterError } from "$lib/utils/utils";
  import { analytics } from "$lib/utils/analytics/analytics";
  import type {
    EmailChallengeDnsInput,
    EmailChallengeSubmitDkimLeafArg,
    VerifiedEmail,
  } from "$lib/generated/internet_identity_types";

  interface Props {
    verifiedEmails: VerifiedEmail[];
    capacity: number;
    recoveryAddresses?: string[];
    openidAddresses?: string[];
  }

  const {
    verifiedEmails,
    capacity,
    recoveryAddresses = [],
    openidAddresses = [],
  }: Props = $props();

  let showAddWizard = $state(false);
  let removingAddress = $state<string | undefined>(undefined);

  // Aliased so the lingui-extracted placeholder name stays `count` —
  // renaming `verifiedEmails.length` would orphan translations.
  const count = $derived(verifiedEmails.length);
  const isFull = $derived(count >= capacity);

  const prepareAddVerifiedEmail = (input: EmailChallengeDnsInput) =>
    $authenticatedStore.actor
      .verified_email_prepare_add($authenticatedStore.identityNumber, input)
      .then(throwCanisterError);

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

  const handleAddSuccess = (address: string) => {
    showAddWizard = false;
    void invalidateAll();
    toaster.success({
      title: $t`Email address verified`,
      description: $t`${address} has been associated with your Internet Identity.`,
    });
  };

  const handleAddClosed = () => {
    showAddWizard = false;
    void invalidateAll();
  };

  const handleRemove = async (address: string) => {
    try {
      await $authenticatedStore.actor
        .verified_email_remove($authenticatedStore.identityNumber, address)
        .then(throwCanisterError);
    } catch (err) {
      handleError(err);
      return;
    }
    analytics.event("verified-email-removed");
    removingAddress = undefined;
    void invalidateAll();
    toaster.success({
      title: $t`Email address removed`,
      description: $t`${address} is no longer associated with your Internet Identity.`,
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
        >
          {$t`Email addresses`}
        </h2>
        <span
          class="border-border-tertiary text-text-secondary inline-flex items-center rounded-full border px-2 py-0.5 text-xs font-medium"
          aria-label={$t`${count} of ${capacity} verified emails`}
        >
          {count}/{capacity}
        </span>
      </div>
      <p class="text-text-tertiary text-sm">
        <Trans>
          Manage your shareable emails. These should be different from your
          recovery email.
        </Trans>
      </p>
    </div>
    {#if count > 0}
      <button
        class="btn btn-primary btn-sm max-sm:btn-icon shrink-0"
        onclick={() => (showAddWizard = true)}
        disabled={isFull}
        aria-label={$t`Add an email`}
      >
        <PlusIcon class="size-4" aria-hidden="true" />
        <span class="max-sm:hidden">{$t`Add an email`}</span>
      </button>
    {/if}
  </div>

  {#if count === 0}
    <button
      onclick={() => (showAddWizard = true)}
      aria-label={$t`Add an email`}
      class="border-border-tertiary bg-bg-primary hover:border-border-secondary hover:bg-bg-primary_hover flex flex-col items-center justify-center gap-2 rounded-sm border border-dashed px-6 py-10 text-center transition-colors duration-200 outline-none"
    >
      <MailIcon class="text-fg-secondary size-7" aria-hidden="true" />
      <p aria-hidden="true" class="text-text-tertiary text-sm">
        {$t`No emails yet`}
      </p>
      <span
        aria-hidden="true"
        class="text-text-primary mt-4 inline-flex items-center gap-1.5 text-sm font-semibold"
      >
        <PlusIcon class="size-4" aria-hidden="true" />
        {$t`Add an email`}
      </span>
    </button>
  {:else}
    <ul class="flex flex-col gap-2">
      {#each verifiedEmails as entry (entry.address)}
        {@const verifiedAt = new Date(nanosToMillis(entry.verified_at))}
        <li
          class="bg-bg-secondary border-border-secondary relative flex flex-row items-center gap-3 rounded-xl border px-4 py-3"
        >
          <div class="flex min-w-0 flex-1 flex-col gap-1.5 overflow-hidden">
            <span
              class="text-text-primary min-w-0 truncate text-sm font-semibold"
            >
              {entry.address}
            </span>
            <div class="flex flex-row flex-wrap items-center gap-x-2.5 gap-y-1">
              <span
                class="border-fg-success-primary bg-bg-success-primary text-text-success-primary inline-flex shrink-0 items-center gap-1.5 rounded-full border px-2 py-0.5 text-xs font-medium"
              >
                <span
                  class="bg-fg-success-primary size-1.5 rounded-full"
                  aria-hidden="true"
                ></span>
                {$t`Verified`}
              </span>
              <time
                datetime={verifiedAt.toISOString()}
                title={$formatDate(verifiedAt, {
                  timeStyle: "short",
                  dateStyle: "medium",
                })}
                class="text-text-tertiary text-sm"
              >
                {$formatRelative(verifiedAt, { style: "long" })}
              </time>
            </div>
          </div>
          <button
            class="btn btn-tertiary btn-sm btn-icon relative z-10 shrink-0"
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
      {recoveryAddresses}
      verifiedAddresses={verifiedEmails.map((e) => e.address)}
      {openidAddresses}
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
