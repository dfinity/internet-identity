<script lang="ts">
  import { goto } from "$app/navigation";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import MigrationWizard from "$lib/components/wizards/MigrationWizard.svelte";
</script>

<div class="flex min-h-[100dvh] flex-col" data-page="migration-view">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <div class="col-start-1 row-start-1 flex flex-col">
      <AuthPanel>
        <MigrationWizard onSuccess={() => goto("/manage")} />
      </AuthPanel>
    </div>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>
