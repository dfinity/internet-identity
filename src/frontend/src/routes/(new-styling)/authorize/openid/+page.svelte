<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import { handleError } from "$lib/components/utils/error";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import { page } from "$app/state";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  const onSignIn = async () => {
    await authorizationStore.authorize(undefined);
  };
  const onSignUp = async () => {
    await authorizationStore.authorize(undefined);
  };

  const continueWithJWT =
    nonNullish(page.state) &&
    "jwt" in page.state &&
    typeof page.state.jwt === "string"
      ? page.state.jwt
      : undefined;
</script>

<AuthWizard {onSignIn} {onSignUp} onError={handleError} {continueWithJWT}>
  <div class="flex flex-col items-center justify-center gap-4">
    <ProgressRing class="text-fg-primary size-14" />
    <p class="text-text-secondary text-lg">Authenticating</p>
  </div>
</AuthWizard>
