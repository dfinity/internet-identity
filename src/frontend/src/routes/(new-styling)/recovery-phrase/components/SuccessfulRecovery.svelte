<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserCheckIcon, UserIcon } from "@lucide/svelte";
  import { formatDate, t } from "$lib/stores/locale.store";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { nonNullish } from "@dfinity/utils";
  import { nanosToMillis } from "$lib/utils/time";

  type Props = {
    identityName: string;
    createdAt?: Date;
    onContinue: () => Promise<void>;
    onCancel: () => void;
  };

  const { identityName, createdAt, onContinue, onCancel }: Props = $props();

  let continueInProgress = $state(false);

  const handleContinue = async () => {
    continueInProgress = true;
    await onContinue();
    continueInProgress = false;
  };
</script>

<div class="flex flex-col gap-8">
  <div class="mt-6 flex flex-col gap-4">
    <FeaturedIcon size="md" variant="success">
      <UserCheckIcon class="size-6" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h2 class="text-text-primary text-xl">
        {$t`We have found your identity`}
      </h2>
      <p class="text-text-tertiary text-sm">
        {$t`Here is the identity that is linked to your recovery phrase. Continue to access it and add another access method if required.`}
      </p>
    </div>
  </div>
  <ButtonCard onclick={onContinue}>
    <Avatar size="sm">
      <UserIcon class="size-5" />
    </Avatar>
    <div class="flex flex-col text-left text-sm">
      <div class="text-text-primary font-semibold">
        {identityName}
      </div>
      <div class="text-text-tertiary font-normal" aria-hidden="true">
        {#if nonNullish(createdAt)}
          <span
            >{$t`Created ${$formatDate(createdAt, { dateStyle: "short" })}`}</span
          >
        {/if}
      </div>
    </div>
  </ButtonCard>
  <div class="align-stretch flex flex-col gap-2">
    <Button size="lg" onclick={handleContinue} disabled={continueInProgress}>
      {$t`Continue`}
    </Button>
    <Button size="lg" variant="tertiary" onclick={onCancel}>
      {$t`Cancel`}
    </Button>
  </div>
</div>
