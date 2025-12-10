<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserCheckIcon, UserIcon } from "@lucide/svelte";
  import { formatDate, t } from "$lib/stores/locale.store";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import type { IdentityInfo } from "$lib/generated/internet_identity_types";
  import { nanosToMillis } from "$lib/utils/time";
  import { Trans } from "$lib/components/locale";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  type Props = {
    identityInfo: IdentityInfo;
    onContinue: () => Promise<void>;
    onCancel: () => void;
  };

  const { identityInfo, onContinue, onCancel }: Props = $props();

  let continueInProgress = $state(false);

  const handleContinue = async () => {
    try {
      continueInProgress = true;
      await onContinue();
    } finally {
      continueInProgress = false;
    }
  };
</script>

<FeaturedIcon variant="success" size="lg" class="mb-4">
  <UserCheckIcon class="size-6" />
</FeaturedIcon>
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`We have found your identity`}
</h2>
<p class="text-text-tertiary mb-5 text-base font-medium text-pretty">
  <Trans>
    Here is the identity that is linked to your recovery phrase. Continue to
    access it and add another access method if required.
  </Trans>
</p>
<div
  class={[
    "mb-5 flex items-center justify-start gap-3 p-3",
    "bg-bg-primary border-border-secondary rounded-md border-1",
  ]}
>
  <Avatar size="sm">
    <UserIcon class="size-5" />
  </Avatar>
  <div class="flex flex-col text-left text-sm">
    <div class="text-text-primary font-semibold">
      {identityInfo.name[0]}
    </div>
    <div class="text-text-tertiary font-normal" aria-hidden="true">
      {#if identityInfo.created_at[0] !== undefined}
        {@const date = $formatDate(
          new Date(nanosToMillis(identityInfo.created_at[0])),
          { dateStyle: "short" },
        )}
        <span>{$t`Created ${date}`}</span>
      {/if}
    </div>
  </div>
</div>
<button
  onclick={handleContinue}
  disabled={continueInProgress}
  class="btn btn-lg mb-3"
>
  {#if continueInProgress}
    <ProgressRing />
  {/if}
  <span>{$t`Continue`}</span>
</button>
<button
  onclick={onCancel}
  disabled={continueInProgress}
  class="btn btn-tertiary btn-lg"
>
  {$t`Cancel`}
</button>
