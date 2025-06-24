<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserIcon } from "@lucide/svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { MigrationFlow } from "$lib/flows/migrationFlow.svelte";
  import CreatePasskey from "$lib/components/views/CreatePasskey.svelte";

  let identityNumber = $state(undefined);
  const migrationFlow = new MigrationFlow();

  const handleSubmit = async () => {
    if (nonNullish(identityNumber)) {
      await migrationFlow.authenticateWithIdentityNumber(identityNumber);
    }
  };

  const handleCreate = async (name: string) => {
    await migrationFlow.createPasskey(name);
  };
</script>

<div class="flex min-h-[100dvh] flex-col" data-page="migration-view">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <div class="col-start-1 row-start-1 flex flex-col">
      <AuthPanel>
        {#if migrationFlow.view === "enterNumber"}
          {@render enterIndentitNumber()}
        {:else if migrationFlow.view === "enterName"}
          <CreatePasskey create={handleCreate} />
        {:else}
          <p>Migration completed</p>
        {/if}
      </AuthPanel>
    </div>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

{#snippet enterIndentitNumber()}
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
        disabled={isNullish(identityNumber)}
      >
        <span>Migrate Identity</span>
      </Button>
    </div>
  </form>
{/snippet}
