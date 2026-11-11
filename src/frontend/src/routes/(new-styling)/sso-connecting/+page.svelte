<script lang="ts">
  import SsoConnectingView from "$lib/components/views/SsoConnectingView.svelte";
</script>

<!-- The SSO popup's first page, open while discovery resolves; the opener then
     navigates this window to the IdP. -->
<div class="flex min-h-dvh items-center justify-center">
  <SsoConnectingView />
</div>
