<script lang="ts">
  import CheckIcon from "@lucide/svelte/icons/check";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";

  const {
    url,
    onDone,
  }: {
    /** Where the install lives, for a browser that would not open it in a tab. */
    url: string | undefined;
    onDone: () => void;
  } = $props();
</script>

<!-- Shown where the tab was refused. Sign-in is already finished, which is why this
     says so first: whatever the user does from here, they are signed in. -->
<div class="flex min-w-0 flex-col items-stretch">
  <FeaturedIcon size="lg" class="mb-4 self-start">
    <CheckIcon class="size-6" aria-hidden="true" />
  </FeaturedIcon>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">
    {$t`You're signed in`}
  </h1>
  <p class="text-text-tertiary mb-5 text-base">
    <Trans>
      Notifications need one more step on iPhone and iPad. Open the link below
      and follow it to the end.
    </Trans>
  </p>

  <div class="flex flex-col gap-2.5">
    {#if url !== undefined}
      <a
        class="btn btn-primary btn-xl"
        href={url}
        target="_blank"
        rel="noreferrer"
      >
        {$t`Set up notifications`}
      </a>
    {/if}
    <button class="btn btn-tertiary btn-xl" onclick={onDone}>
      {$t`Continue`}
    </button>
  </div>
</div>
