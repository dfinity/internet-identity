<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { LogOutIcon } from "@lucide/svelte";
  import FeaturedIcon from "./FeaturedIcon.svelte";
  import ProgressRing from "./ProgressRing.svelte";

  type Props = {
    /** Signs out and leaves the identity here, so coming back is one tap. */
    onRemember: () => void;
    /** Signs out and takes this browser's local state with it, including the app
     *  sessions it holds — which is a canister call, hence the pending state and hence
     *  both buttons locked while it runs: leaving by the other one would navigate out
     *  from under the revoke. */
    onForget: () => Promise<void>;
  };

  let { onRemember, onForget }: Props = $props();

  let isForgetting = $state(false);

  const handleForget = async () => {
    isForgetting = true;
    try {
      await onForget();
    } finally {
      isForgetting = false;
    }
  };
</script>

<div class="flex flex-col gap-8">
  <div class="flex flex-col gap-4">
    <FeaturedIcon size="lg">
      <LogOutIcon class="size-6" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h2 class="text-text-primary text-2xl font-medium">
        {$t`Remember this browser?`}
      </h2>
      <p class="text-text-tertiary text-base">
        <Trans>
          Your apps stay signed in and your identity is ready the next time you
          come back.
        </Trans>
      </p>
    </div>
  </div>

  <div class="flex flex-col gap-3">
    <button onclick={onRemember} class="btn w-full" disabled={isForgetting}>
      {$t`Remember`}
    </button>
    <button
      onclick={handleForget}
      class="btn btn-tertiary w-full"
      disabled={isForgetting}
    >
      {#if isForgetting}
        <ProgressRing />
        <span>{$t`Forgetting...`}</span>
      {:else}
        <span>{$t`Forget`}</span>
      {/if}
    </button>
  </div>
</div>
