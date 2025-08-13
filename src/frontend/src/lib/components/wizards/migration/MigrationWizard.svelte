<script lang="ts">
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { MigrationFlow } from "$lib/flows/migrationFlow.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import EnterIdentityNumber from "./views/EnterIdentityNumber.svelte";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";

  interface Props {
    onSuccess: (identityNumber: bigint) => void;
    onError: (error: unknown) => void;
  }

  const { onSuccess, onError }: Props = $props();

  const migrationFlow = new MigrationFlow();

  const handleSubmit = async (
    identityNumber: bigint,
    attachElement?: HTMLElement,
  ) => {
    try {
      await migrationFlow.authenticateWithIdentityNumber(
        BigInt(identityNumber),
        attachElement,
      );
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
        throw error; // Error is handled by child component
      } else {
        onError(error); // Propagate unhandled errors to parent component
      }
    }
  };

  const handleCreate = async (name: string) => {
    if (isNullish(migrationFlow.identityNumber)) {
      // Button is disabled if identityNumber is null or undefined so no need to manage that case.
      throw new Error("Identity number is undefined");
    }
    try {
      await migrationFlow.createPasskey(name);
      onSuccess(migrationFlow.identityNumber);
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
        throw error; // Error is handled by child component
      } else {
        onError(error); // Propagate unhandled errors to parent component
      }
    }
  };
</script>

{#if migrationFlow.view === "enterNumber"}
  <EnterIdentityNumber onSubmit={handleSubmit} />
  <!-- User can't move to this step if identityNumber is null or undefined so no need to manage that case. -->
{:else if migrationFlow.view === "enterName" && nonNullish(migrationFlow.identityNumber)}
  <CreatePasskey
    create={handleCreate}
    identityNumber={migrationFlow.identityNumber}
  />
{/if}
