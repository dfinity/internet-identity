<script lang="ts">
  import MigrationWizard from "$lib/components/wizards/MigrationWizard.svelte";
  import { authorizationStore } from "$lib/stores/authorization.store";
</script>

<MigrationWizard
  onSuccess={() => authorizationStore.authorize(undefined, 4000)}
/>
