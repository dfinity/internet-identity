<script lang="ts">
  import { goto } from "$app/navigation";
  import MigrationWizard from "$lib/components/wizards/MigrationWizard.svelte";
</script>

<MigrationWizard onSuccess={() => goto("/manage")} />
