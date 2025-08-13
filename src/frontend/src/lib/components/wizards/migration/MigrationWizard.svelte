<script lang="ts">
  import { isNullish, nonNullish } from "@dfinity/utils";
  import {
    MigrationFlow,
    WrongDomainError,
  } from "$lib/flows/migrationFlow.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import EnterIdentityNumber from "./views/EnterIdentityNumber.svelte";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import { isLegacyAuthnMethod } from "$lib/utils/accessMethods";
  import AlreadyMigrated from "./views/AlreadyMigrated.svelte";

  interface Props {
    onSuccess: (identityNumber: bigint) => void;
    onError: (error: unknown) => void;
  }

  const { onSuccess, onError }: Props = $props();

  const migrationFlow = new MigrationFlow();
  const alreadyMigrated = $derived(
    identityInfo.loaded &&
      identityInfo.authnMethods.some(
        (authnMethod) => !isLegacyAuthnMethod(authnMethod),
      ),
  );

  const handleSubmit = async (
    identityNumber: bigint,
    attachElement?: HTMLElement,
  ): Promise<void | "cancelled" | "wrongDomain"> => {
    try {
      await migrationFlow.authenticateWithIdentityNumber(
        BigInt(identityNumber),
        attachElement,
      );
      // Fetch the identity info to check whether it has already been migrated or not.
      await identityInfo.fetch();
      migrationFlow.view = "enterName";
    } catch (error) {
      if (error instanceof WrongDomainError) {
        return "wrongDomain";
      }
      if (isWebAuthnCancelError(error)) {
        return "cancelled";
      }
      onError(error); // Propagate unhandled errors to parent component
    }
  };

  const handleCreate = async (name: string): Promise<void | "cancelled"> => {
    if (isNullish(migrationFlow.identityNumber)) {
      // Button is disabled if identityNumber is null or undefined so no need to manage that case.
      throw new Error("Identity number is undefined");
    }
    try {
      await migrationFlow.createPasskey(name);
      onSuccess(migrationFlow.identityNumber);
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
        return "cancelled";
      }
      onError(error); // Propagate unhandled errors to parent component
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
