<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import MigrationSuccessIllustration from "$lib/components/illustrations/MigrationSuccessIllustration.svelte";

  const handleRedirect = () => {
    authorizationStore.authorize(undefined);
  };
</script>

<div
  class="flex flex-1 flex-col items-stretch justify-end p-4 sm:max-w-100 sm:items-center sm:justify-center"
>
  <div
    class="text-text-primary flex h-50 flex-1 items-center justify-center sm:flex-none"
  >
    <MigrationSuccessIllustration />
  </div>
  <div class="mb-8 flex flex-col gap-2">
    <h1 class="text-text-primary mb-3 text-center text-2xl font-medium">
      You're all set!
    </h1>
    <p class="text-md text-text-tertiary text-center font-medium text-balance">
      Your identity has been successfully migrated to the new experience.
    </p>
  </div>
  <Button variant="primary" onclick={handleRedirect}>Go to app</Button>
</div>
