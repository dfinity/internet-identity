<script lang="ts">
  /** Diagnostic, on beta only: what the worker recorded about a notification click,
   *  on a page rather than in a console a browser may not offer. Remove with the
   *  tracing in `service-worker.ts` once the desktop behaviour is confirmed. */
  import { onMount } from "svelte";

  let lines = $state<string>("reading...");
  let worker = $state<string>("");

  const read = async () => {
    try {
      const registration = await navigator.serviceWorker.getRegistration();
      worker = JSON.stringify(
        {
          active: registration?.active?.scriptURL,
          state: registration?.active?.state,
          waiting: registration?.waiting !== null,
          installing: registration?.installing !== null,
        },
        null,
        2,
      );

      const log = await caches.open("ii-notification-trace");
      const keys = await log.keys();
      const entries = await Promise.all(
        keys.map(async (key) => (await log.match(key))?.json()),
      );
      lines =
        entries.length === 0
          ? "no clicks recorded yet"
          : entries.map((entry) => JSON.stringify(entry, null, 2)).join("\n\n");
    } catch (error) {
      lines = `could not read: ${String(error)}`;
    }
  };

  const clear = async () => {
    await caches.delete("ii-notification-trace");
    await read();
  };

  onMount(() => {
    void read();
  });
</script>

<svelte:head>
  <title>Notification trace</title>
</svelte:head>

<main class="flex min-h-dvh flex-col gap-4 p-6">
  <div class="flex gap-2">
    <button class="btn btn-primary" onclick={() => void read()}>Refresh</button>
    <button class="btn btn-secondary" onclick={() => void clear()}>Clear</button
    >
  </div>
  <pre
    class="text-text-primary overflow-auto text-xs whitespace-pre-wrap">{worker}</pre>
  <pre
    class="text-text-primary overflow-auto text-xs whitespace-pre-wrap">{lines}</pre>
</main>
