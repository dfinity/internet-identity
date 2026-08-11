<script lang="ts">
  import { ImageIcon, PencilIcon, Trash2Icon } from "@lucide/svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { invalidateAll } from "$app/navigation";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import RemoveProfilePicture from "./RemoveProfilePicture.svelte";
  import { Trans } from "$lib/components/locale";
  import { formatDate, formatRelative, t } from "$lib/stores/locale.store";
  import { toaster } from "$lib/components/utils/toaster";
  import { handleError } from "$lib/components/utils/error";
  import { nanosToMillis } from "$lib/utils/time";
  import { throwCanisterError } from "$lib/utils/utils";
  import { analytics } from "$lib/utils/analytics/analytics";
  import {
    PROFILE_PICTURE_ACCEPT,
    PROFILE_PICTURE_MAX_BYTES,
    ProfilePictureError,
    prepareProfilePicture,
    profilePictureDataUrl,
  } from "$lib/utils/profilePicture";
  import type { ProfilePictureMetadata } from "$lib/generated/internet_identity_types";

  interface Props {
    /** From `identity_info`; `undefined` when no picture is set. The bytes are
     *  deliberately not in that response, so this panel fetches them itself. */
    metadata?: ProfilePictureMetadata;
  }

  const { metadata }: Props = $props();

  let fileInput = $state<HTMLInputElement | undefined>();
  let isSaving = $state(false);
  let showRemoveDialog = $state(false);

  const hasPicture = $derived(metadata !== undefined);
  const uploadedAt = $derived(
    metadata === undefined
      ? undefined
      : new Date(nanosToMillis(metadata.uploaded_at)),
  );

  /** The bytes, fetched lazily and only when a picture exists. Kept as a
   *  promise so the template can render a skeleton via `{#await}` — the
   *  fetch is a full update call, so it is not instant. Re-created whenever
   *  `metadata` changes, which is what makes `invalidateAll()` after a save
   *  refresh the preview. */
  const pictureUrl = $derived.by(async (): Promise<string | undefined> => {
    if (metadata === undefined) return undefined;
    const picture = await $authenticatedStore.actor
      .profile_picture_get($authenticatedStore.identityNumber)
      .then(throwCanisterError);
    return picture[0] === undefined
      ? undefined
      : profilePictureDataUrl(picture[0]);
  });

  const messageFor = (error: unknown): string | undefined => {
    if (!(error instanceof ProfilePictureError)) return undefined;
    switch (error.detail.kind) {
      case "unsupported-type":
        return $t`Choose a PNG, JPEG or WebP image.`;
      case "source-too-large":
        return $t`That image is too large to open. Choose one under 20 MB.`;
      case "decode-failed":
        return $t`That file couldn't be opened as an image.`;
      case "encode-failed":
      case "still-too-large":
        return $t`That image couldn't be resized to fit. Try a different one.`;
    }
  };

  const handleFileChosen = async (event: Event) => {
    const input = event.currentTarget as HTMLInputElement;
    const file = input.files?.[0];
    // Reset immediately so picking the same file twice in a row still fires
    // a `change` event.
    input.value = "";
    if (file === undefined) return;

    isSaving = true;
    try {
      const prepared = await prepareProfilePicture(file);
      await $authenticatedStore.actor
        .profile_picture_set($authenticatedStore.identityNumber, {
          bytes: prepared.bytes,
        })
        .then(throwCanisterError);
      analytics.event("profile-picture-set");
      void invalidateAll();
      toaster.success({
        title: $t`Profile picture saved`,
        description: $t`Apps you share it with will see this picture.`,
      });
    } catch (error) {
      const message = messageFor(error);
      if (message === undefined) {
        handleError(error);
      } else {
        toaster.error({
          title: $t`Couldn't use that image`,
          description: message,
        });
      }
    } finally {
      isSaving = false;
    }
  };

  const handleRemove = async () => {
    try {
      await $authenticatedStore.actor
        .profile_picture_remove($authenticatedStore.identityNumber)
        .then(throwCanisterError);
    } catch (error) {
      handleError(error);
      return;
    }
    analytics.event("profile-picture-removed");
    showRemoveDialog = false;
    void invalidateAll();
    toaster.success({
      title: $t`Profile picture removed`,
      description: $t`It is no longer associated with your Internet Identity.`,
    });
  };
</script>

<section class="flex flex-col gap-4" aria-labelledby="profile-picture-heading">
  <div class="flex flex-col gap-1">
    <h2
      id="profile-picture-heading"
      class="text-text-primary text-lg font-semibold"
    >
      {$t`Profile picture`}
    </h2>
    <p class="text-text-tertiary text-sm">
      <Trans>
        One picture, shared only with apps you allow. It's resized before it
        leaves your device.
      </Trans>
    </p>
  </div>

  <!-- The one input; both the empty-state tile and the "Change" button open
       it, so there is a single upload path to reason about. -->
  <input
    bind:this={fileInput}
    type="file"
    accept={PROFILE_PICTURE_ACCEPT}
    onchange={handleFileChosen}
    class="hidden"
    aria-hidden="true"
    tabindex="-1"
  />

  {#if !hasPicture}
    <button
      onclick={() => fileInput?.click()}
      disabled={isSaving}
      aria-label={$t`Add a picture`}
      class="border-border-tertiary bg-bg-primary hover:border-border-secondary hover:bg-bg-primary_hover flex flex-col items-center justify-center gap-2 rounded-sm border border-dashed px-6 py-10 text-center transition-colors duration-200 outline-none"
    >
      {#if isSaving}
        <ProgressRing />
        <p aria-hidden="true" class="text-text-tertiary text-sm">
          {$t`Saving…`}
        </p>
      {:else}
        <ImageIcon class="text-fg-secondary size-7" aria-hidden="true" />
        <p aria-hidden="true" class="text-text-tertiary text-sm">
          {$t`No picture yet`}
        </p>
        <span
          aria-hidden="true"
          class="text-text-primary mt-4 inline-flex items-center gap-1.5 text-sm font-semibold"
        >
          <PencilIcon class="size-4" aria-hidden="true" />
          {$t`Add a picture`}
        </span>
      {/if}
    </button>
  {:else}
    <div
      class="bg-bg-secondary border-border-secondary flex flex-row items-center gap-4 rounded-xl border px-4 py-3"
    >
      {#await pictureUrl}
        <div class="skeleton size-14 shrink-0 rounded-full"></div>
      {:then url}
        {#if url === undefined}
          <div
            class="bg-bg-tertiary text-fg-tertiary flex size-14 shrink-0 items-center justify-center rounded-full"
          >
            <ImageIcon class="size-6" aria-hidden="true" />
          </div>
        {:else}
          <img
            src={url}
            alt={$t`Your profile picture`}
            class="bg-bg-tertiary size-14 shrink-0 rounded-full object-cover"
          />
        {/if}
      {:catch}
        <div
          class="bg-bg-tertiary text-fg-tertiary flex size-14 shrink-0 items-center justify-center rounded-full"
        >
          <ImageIcon class="size-6" aria-hidden="true" />
        </div>
      {/await}

      <div class="flex min-w-0 flex-1 flex-col gap-1.5">
        <span class="text-text-primary text-sm font-semibold">
          {$t`Shareable`}
        </span>
        {#if uploadedAt !== undefined}
          <time
            datetime={uploadedAt.toISOString()}
            title={$formatDate(uploadedAt, {
              timeStyle: "short",
              dateStyle: "medium",
            })}
            class="text-text-tertiary text-sm"
          >
            {$formatRelative(uploadedAt, { style: "long" })}
          </time>
        {/if}
      </div>

      <div class="flex shrink-0 flex-row items-center gap-1">
        <button
          class="btn btn-tertiary btn-sm btn-icon"
          onclick={() => fileInput?.click()}
          disabled={isSaving}
          aria-label={$t`Change your profile picture`}
        >
          {#if isSaving}
            <ProgressRing />
          {:else}
            <PencilIcon class="size-4" aria-hidden="true" />
          {/if}
        </button>
        <button
          class="btn btn-tertiary btn-sm btn-icon"
          onclick={() => (showRemoveDialog = true)}
          disabled={isSaving}
          aria-label={$t`Remove your profile picture`}
        >
          <Trash2Icon class="size-4" aria-hidden="true" />
        </button>
      </div>
    </div>
  {/if}

  <p class="text-text-tertiary text-xs">
    <Trans>
      PNG, JPEG or WebP. Stored at up to {PROFILE_PICTURE_MAX_BYTES / 1024} KB after
      resizing.
    </Trans>
  </p>
</section>

{#if showRemoveDialog}
  <Dialog onClose={() => (showRemoveDialog = false)}>
    <RemoveProfilePicture
      onRemove={handleRemove}
      onCancel={() => (showRemoveDialog = false)}
    />
  </Dialog>
{/if}
