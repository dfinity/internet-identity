<script lang="ts">
  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import Button from "$lib/components/UI/Button.svelte";
</script>

<!-- an element with id 'newAuthenticateTitle' is necessary for the e2e tests to pass -->
<CenterContainer data-role="new-authenticate-view">
  <div class="flex flex-col gap-2">
    <div class="flex flex-col gap-1">
      <h1 class="h1 font-bold">[Sign in]</h1>
      <h2 class="h2">to your Internet Identity</h2>
    </div>
    <Button class="w-full" variant="primary">Continue with Passkey</Button>
    <Button class="w-full" variant="secondary">Continue with Google</Button>
  </div>
</CenterContainer>
