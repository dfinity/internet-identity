<script lang="ts">
  import Input from "$lib/components/ui/Input.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { MigrationFlow } from "$lib/flows/migrationFlow.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { handleError } from "$lib/components/utils/error";
  import FeaturedIcon from "../ui/FeaturedIcon.svelte";
  import { UserIcon } from "@lucide/svelte";

  const { onSuccess }: { onSuccess: () => void } = $props();

  let identityNumber: number | undefined = $state(undefined);
  const migrationFlow = new MigrationFlow();

  const handleSubmit = async () => {
    // Button is disabled if identityNumber is null or undefined so no need to manage that case.
    if (nonNullish(identityNumber)) {
      await migrationFlow
        .authenticateWithIdentityNumber(BigInt(identityNumber))
        .catch(handleError);
    }
  };

  const handleCreate = async (name: string) => {
    try {
      await migrationFlow.createPasskey(name, true);
      onSuccess();
    } catch (error) {
      handleError(error);
    }
  };
</script>

{#if migrationFlow.view === "enterNumber"}
  {@render enterIdentityNumber()}
{:else if migrationFlow.view === "enterName"}
  <CreatePasskey create={handleCreate} />
{/if}

{#snippet enterIdentityNumber()}
  <form class="flex flex-1 flex-col">
    <div class="mb-8 flex flex-col">
      <FeaturedIcon size="lg" class="mb-4 self-start">
        <UserIcon size="1.5rem" />
      </FeaturedIcon>
      <h1 class="text-text-primary mb-3 text-2xl font-medium">
        Enter the identity number
      </h1>
      <p class="text-md text-text-tertiary mb-6 font-medium">
        This process will allow you to use the new authentication flow.
      </p>
      <Input
        bind:value={identityNumber}
        inputmode="numeric"
        placeholder="Identity number"
        type="number"
        size="md"
        autocomplete="off"
        autocorrect="off"
        spellcheck="false"
        aria-label="Identity number"
      />
    </div>
    <div class="mt-auto flex flex-col items-stretch gap-3">
      <Button
        onclick={handleSubmit}
        variant="primary"
        size="lg"
        type="submit"
        disabled={isNullish(identityNumber) || migrationFlow.authenticating}
      >
        {#if migrationFlow.authenticating}
          <ProgressRing />
          <span>Authenticating...</span>
        {:else}
          <span>Migrate Identity</span>
        {/if}
      </Button>
    </div>
  </form>
{/snippet}
