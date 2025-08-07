<script lang="ts">
  import { MigrationWizard } from "$lib/components/wizards/migration";
  import { authorizationStore } from "$lib/stores/authorization.store";
</script>

<MigrationWizard
  onSuccess={() => authorizationStore.authorize(undefined, 4000)}
/>
