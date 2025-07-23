<script lang="ts">
  import { onMount } from "svelte";
  import { goto } from "$app/navigation";
  import { toaster } from "$lib/components/utils/toaster";

  // Confirming a passkey from another device is handled in the dashboard,
  // this can't be a load function since URL hash can't be read in there.
  onMount(async () => {
    const code = window.location.hash.slice(1);
    const isValid = code.length === 5;
    await goto(
      isValid ? `/manage?activate=${window.location.hash.slice(1)}` : "/manage",
      {
        replaceState: true,
      },
    );
    if (!isValid) {
      toaster.error({
        title: "Invalid code link",
        description: "We didn't recognize that activation code.",
      });
    }
  });
</script>
