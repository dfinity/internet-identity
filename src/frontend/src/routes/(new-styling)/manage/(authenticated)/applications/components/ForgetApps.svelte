<script lang="ts">
  import { TriangleAlertIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { plural, t } from "$lib/stores/locale.store";
  import { appNameStore } from "../apps";

  interface Props {
    titleId: string;
    origins: [string, ...string[]];
    onForget: () => Promise<void>;
    onCancel: () => void;
  }

  const { titleId, origins, onForget, onCancel }: Props = $props();

  const nameStore = $derived(appNameStore(origins[0]));
  const name = $derived($nameStore);
  const single = $derived(origins.length === 1);

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

<div class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon variant="warning" size="lg" class="mb-3">
      <TriangleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 id={titleId} class="text-text-primary mb-3 text-2xl font-medium">
      {single
        ? $t`Remove ${name}?`
        : $plural(origins.length, {
            one: "Remove 1 app?",
            other: "Remove # apps?",
          })}
    </h1>
    <div
      class={[
        "flex flex-col gap-4",
        "[&_p]:text-text-tertiary [&_p]:text-base [&_p]:font-medium",
      ]}
    >
      <p>
        {single
          ? $t`You'll be signed out of ${name} and it will no longer be able to notify you.`
          : $t`You'll be signed out of these apps and they will no longer be able to notify you.`}
      </p>
      <p>
        {single
          ? $t`You can sign in to it again at any time.`
          : $t`You can sign in to them again at any time.`}
      </p>
    </div>
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <button
      class="btn btn-primary btn-lg btn-danger"
      onclick={handleForget}
      disabled={isForgetting}
    >
      {#if isForgetting}
        <ProgressRing />
        <span>{$t`Removing...`}</span>
      {:else}
        <span>{single ? $t`Remove app` : $t`Remove apps`}</span>
      {/if}
    </button>
    <button
      class="btn btn-tertiary btn-lg"
      onclick={onCancel}
      disabled={isForgetting}
    >
      {$t`Cancel`}
    </button>
  </div>
</div>
