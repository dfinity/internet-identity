<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import { MigrationFlow } from "$lib/flows/migrationFlow.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import { handleError } from "$lib/components/utils/error";
  import EnterIdentityNumber from "./views/EnterIdentityNumber.svelte";
  import CreatePasskeyMigration from "./views/CreatePasskeyMigration.svelte";

  const { onSuccess }: { onSuccess: (identityNumber: bigint) => void } =
    $props();

  const migrationFlow = new MigrationFlow();

  const handleSubmit = async (identityNumber: bigint) => {
    await migrationFlow
      .authenticateWithIdentityNumber(BigInt(identityNumber))
      .catch(handleError);
  };

  const handleCreate = async (name: string) => {
    await migrationFlow.createPasskey(name);
    // Button is disabled if identityNumber is null or undefined so no need to manage that case.
    if (nonNullish(migrationFlow.identityNumber)) {
      onSuccess(migrationFlow.identityNumber);
    }
  };
</script>

{#if migrationFlow.view === "enterNumber"}
  <EnterIdentityNumber
    onSubmit={handleSubmit}
    isAuthenticating={migrationFlow.authenticating}
  />
  <!-- User can't move to this step if identityNumber is null or undefined so no need to manage that case. -->
{:else if migrationFlow.view === "enterName" && nonNullish(migrationFlow.identityNumber)}
  <CreatePasskeyMigration
    onSubmit={handleCreate}
    isAuthenticating={migrationFlow.authenticating}
    identityNumber={migrationFlow.identityNumber}
  />
{/if}
