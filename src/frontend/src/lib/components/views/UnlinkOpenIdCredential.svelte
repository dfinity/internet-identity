<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    onUnlink: () => Promise<void>;
    onCancel: () => void;
    providerName: string;
    isCurrentAccessMethod?: boolean;
  }

  const {
    onUnlink,
    onCancel,
    providerName: name,
    isCurrentAccessMethod,
  }: Props = $props();

  let isUnlinking = $state(false);

  const handleUnlink = async () => {
    try {
      isUnlinking = true;
      await onUnlink();
    } finally {
      isUnlinking = false;
    }
  };
</script>

<div class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon variant="warning" size="lg" class="mb-3">
      <TriangleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Are you sure?`}
    </h1>
    <div
      class={[
        "flex flex-col gap-4",
        "[&_p]:text-text-tertiary [&_p]:text-base [&_p]:font-medium",
      ]}
    >
      <p>
        <Trans>
          You're about to unlink your {name} account.
        </Trans>
      </p>
      <p>
        <Trans>
          If you proceed, you will no longer be able to sign-in to your identity
          or dapps using your {name}
          account.
        </Trans>
      </p>
      {#if isCurrentAccessMethod}
        <p>
          <Trans>
            As you are currently signed in with this Account, you will be signed
            out.
          </Trans>
        </p>
      {/if}
    </div>
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <Button onclick={handleUnlink} size="lg" danger disabled={isUnlinking}>
      {#if isUnlinking}
        <ProgressRing />
        <span>{$t`Unlinking ${name} account...`}</span>
      {:else}
        <span>{$t`Unlink ${name} account`}</span>
      {/if}
    </Button>
    <Button
      onclick={onCancel}
      variant="tertiary"
      size="lg"
      disabled={isUnlinking}
    >
      {$t`Keep linked`}
    </Button>
  </div>
</div>
