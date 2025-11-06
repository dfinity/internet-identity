<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    onRemove: () => Promise<void>;
    onCancel: () => void;
    providerName: string;
    isCurrentAccessMethod?: boolean;
  }

  const {
    onRemove,
    onCancel,
    providerName: name,
    isCurrentAccessMethod,
  }: Props = $props();

  let isRemoving = $state(false);

  const handleRemove = async () => {
    try {
      isRemoving = true;
      await onRemove();
    } finally {
      isRemoving = false;
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
    <Button onclick={handleRemove} size="lg" danger disabled={isRemoving}>
      {#if isRemoving}
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
      disabled={isRemoving}
    >
      {$t`Keep linked`}
    </Button>
  </div>
</div>
