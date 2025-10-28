<script lang="ts">
  import { isNullish, nonNullish } from "@dfinity/utils";
  import {
    MigrationFlow,
    WrongDomainError,
  } from "$lib/flows/migrationFlow.svelte";
  import UpgradePasskey from "./views/UpgradePasskey.svelte";
  import EnterIdentityNumber from "./views/EnterIdentityNumber.svelte";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
  import AlreadyMigrated from "./views/AlreadyMigrated.svelte";
  import { onMount } from "svelte";
  import {
    UpgradeIdentityEvents,
    upgradeIdentityFunnel,
  } from "$lib/utils/analytics/upgradeIdentityFunnel";

  interface Props {
    onSuccess: (identityNumber: bigint) => void;
    onError: (error: unknown) => void;
  }

  const { onSuccess, onError }: Props = $props();

  onMount(() => {
    upgradeIdentityFunnel.init();
  });

  const migrationFlow = new MigrationFlow();

  const handleSubmit = async (
    identityNumber: bigint,
    attachElement?: HTMLElement,
  ): Promise<void | "cancelled" | "wrongDomain"> => {
    try {
      await migrationFlow.authenticateWithIdentityNumber(
        BigInt(identityNumber),
        attachElement,
      );
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

  const handleUpgrade = async (name: string): Promise<void | "cancelled"> => {
    if (isNullish(migrationFlow.identityNumber)) {
      // Button is disabled if identityNumber is null or undefined so no need to manage that case.
      throw new Error("Identity number is undefined");
    }
    try {
      await migrationFlow.createPasskey(name);
      upgradeIdentityFunnel.trigger(UpgradeIdentityEvents.UpgradeSuccessful);
      upgradeIdentityFunnel.close();
      onSuccess(migrationFlow.identityNumber);
    } catch (error) {
      upgradeIdentityFunnel.trigger(UpgradeIdentityEvents.UpgradeFailure);
      if (isWebAuthnCancelError(error)) {
        return "cancelled";
      }
      onError(error); // Propagate unhandled errors to parent component
    }
  };
</script>

{#if migrationFlow.view === "alreadyMigrated"}
  <AlreadyMigrated onUpgradeAgain={migrationFlow.upgradeAgain} />
{:else if migrationFlow.view === "enterNumber"}
  <EnterIdentityNumber onSubmit={handleSubmit} />
  <!-- User can't move to this step if identityNumber is null or undefined so no need to manage that case. -->
{:else if migrationFlow.view === "enterName" && nonNullish(migrationFlow.identityNumber)}
  <UpgradePasskey
    upgrade={handleUpgrade}
    identityNumber={migrationFlow.identityNumber}
  />
{/if}
