<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onRemove: () => Promise<void>;
    onCancel: () => void;
    type: "passkey" | "openid";
    providerName?: string;
    isCurrentAccessMethod?: boolean;
    isLastAccessMethod?: boolean;
  }

  const {
    onRemove,
    onCancel,
    type,
    providerName,
    isCurrentAccessMethod = false,
    isLastAccessMethod = false,
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
      {#if isLastAccessMethod}
        <p>
          {#if type === "openid"}
            <Trans>
              You are about to unlink your last remaining access method. Your
              recovery phrase will become your sole method of regaining access
              to this identity.
            </Trans>
          {:else}
            <Trans>
              You are about to remove your last remaining access method. Your
              recovery phrase will become your sole method of regaining access
              to this identity.
            </Trans>
          {/if}
        </p>
        {#if isCurrentAccessMethod}
          <p>
            {#if type === "openid"}
              <Trans>
                As you are currently signed in with this access method, you will
                be signed out immediately after unlinking.
              </Trans>
            {:else}
              <Trans>
                As you are currently signed in with this access method, you will
                be signed out immediately after removal.
              </Trans>
            {/if}
          </p>
        {/if}
      {:else if type === "openid"}
        <p>
          <Trans>You're about to unlink your {providerName} account.</Trans>
        </p>
        <p>
          <Trans>
            If you proceed, you will no longer be able to sign-in to your
            identity or dapps using your {providerName} account.
          </Trans>
        </p>
      {:else}
        <p>
          <Trans>
            Removing this passkey means you won't be able to use it to sign in
            anymore. You can always add a new one later.
          </Trans>
        </p>
        <p>
          <Trans>
            It won't be removed from your device or password manager.
          </Trans>
        </p>
      {/if}
    </div>
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <button
      class="btn btn-primary btn-lg btn-danger"
      onclick={handleRemove}
      disabled={isRemoving}
    >
      {#if isRemoving}
        <ProgressRing />
        {#if type === "openid"}
          <span>
            {isLastAccessMethod && isCurrentAccessMethod
              ? $t`Unlinking and signing out...`
              : $t`Unlinking...`}
          </span>
        {:else}
          <span>
            {isLastAccessMethod && isCurrentAccessMethod
              ? $t`Removing and signing out...`
              : $t`Removing passkey...`}
          </span>
        {/if}
      {:else if type === "openid"}
        <span>
          {isLastAccessMethod && isCurrentAccessMethod
            ? $t`Unlink and sign out`
            : $t`Unlink`}
        </span>
      {:else}
        <span>
          {isLastAccessMethod && isCurrentAccessMethod
            ? $t`Remove and sign out`
            : $t`Remove`}
        </span>
      {/if}
    </button>
    <button
      class="btn btn-tertiary btn-lg"
      onclick={onCancel}
      disabled={isRemoving}
    >
      {$t`Cancel`}
    </button>
  </div>
</div>
