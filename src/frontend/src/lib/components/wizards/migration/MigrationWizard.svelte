<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import { MigrationFlow } from "$lib/flows/migrationFlow.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import { handleError } from "$lib/components/utils/error";
  import EnterIdentityNumber from "./views/EnterIdentityNumber.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import { isLegacyAuthnMethod } from "$lib/utils/accessMethods";
  import AlreadyMigrated from "./views/AlreadyMigrated.svelte";

  const { onSuccess }: { onSuccess: (identityNumber: bigint) => void } =
    $props();

  const migrationFlow = new MigrationFlow();
  const alreadyMigrated = $derived(
    identityInfo.authnMethods.some(
      (authnMethod) => !isLegacyAuthnMethod(authnMethod),
    ),
  );

  const handleSubmit = async (
    identityNumber: bigint,
    attachElement?: HTMLElement,
  ) => {
    await migrationFlow
      .authenticateWithIdentityNumber(BigInt(identityNumber), attachElement)
      .catch(handleError);
    // Fetch the identity info to check whether it has already been migrated or not.
    await identityInfo.fetch();
    migrationFlow.view = "enterName";
  };

  const handleCreate = async (name: string) => {
    await migrationFlow.createPasskey(name);
    // Button is disabled if identityNumber is null or undefined so no need to manage that case.
    if (nonNullish(migrationFlow.identityNumber)) {
      onSuccess(migrationFlow.identityNumber);
    }
  };
</script>

{#if alreadyMigrated}
  <AlreadyMigrated name={identityInfo.name} />
{:else if migrationFlow.view === "enterNumber"}
  <EnterIdentityNumber onSubmit={handleSubmit} />
  <!-- User can't move to this step if identityNumber is null or undefined so no need to manage that case. -->
{:else if migrationFlow.view === "enterName" && nonNullish(migrationFlow.identityNumber)}
  <CreatePasskey
    create={handleCreate}
    identityNumber={migrationFlow.identityNumber}
  />
{/if}
