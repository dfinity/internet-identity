<script lang="ts">
  /** Diagnostic, on beta only: what the worker recorded about a notification click,
   *  on a page rather than in a console a browser may not offer. Remove with the
   *  tracing in `service-worker.ts` once the desktop behaviour is confirmed. */
  import { onMount } from "svelte";

  let lines = $state<string>("reading...");
  let worker = $state<string>("");

  const read = async () => {
    try {
      const all = await navigator.serviceWorker.getRegistrations();
      const controlling = await navigator.serviceWorker.getRegistration();
      worker = JSON.stringify(
        {
          registrations: all.map((registration) => ({
            scope: registration.scope,
            state: registration.active?.state,
            controlsThisPage: registration === controlling,
            shownNotifications: undefined as number | undefined,
          })),
        },
        null,
        2,
      );

      // How many notifications each registration still owns: a click goes to the one
      // that showed it, which need not be the one controlling this page.
      const owned = await Promise.all(
        all.map(async (registration) => ({
          scope: registration.scope,
          notifications: (await registration.getNotifications()).map(
            (notification) => ({
              title: notification.title,
              data: notification.data as unknown,
            }),
          ),
        })),
      );
      worker += `\n\n${JSON.stringify(owned, null, 2)}`;

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

  /** A notification shaped like the real thing, shown by the registration controlling
   *  this page, so a click exercises the same handler without waiting for a send. */
  const test = async () => {
    const registration = await navigator.serviceWorker.getRegistration();
    if (registration === undefined) {
      lines = "no registration controls this page";
      return;
    }
    await registration.showNotification("Trace test", {
      body: "Click me",
      data: {
        identityNumber: "10000",
        origin: "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io",
        accountNumber: "",
        appCanisterId: "vt36r-2qaaa-aaaad-aad5a-cai",
        id: "1",
        url: "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io/#chat",
      },
    });
    lines = "test notification shown: click it, then Refresh";
  };

  /** The same notification, five seconds late, so it arrives while this tab is in the
   *  background: that is the one difference between it and a notification a push shows. */
  const testLater = async () => {
    lines = "switch away now: a notification follows in five seconds";
    setTimeout(() => void test(), 5_000);
  };

  /** The same notification, shown by the worker rather than by this page: the one
   *  difference left between a test that dispatches a click and a push that does not. */
  const workerTest = async (withIcon: boolean) => {
    const registration = await navigator.serviceWorker.getRegistration();
    const active = registration?.active;
    if (active === undefined || active === null) {
      lines = "no active worker to ask";
      return;
    }
    active.postMessage({ type: "trace-show", withIcon });
    lines = `asked the worker to show one${withIcon ? " with an icon" : ""}: click it, then Refresh`;
  };

  /** Tag, data and title exactly as a real notification carries them. */
  const realShape = async () => {
    const active = (await navigator.serviceWorker.getRegistration())?.active;
    if (active === undefined || active === null) {
      lines = "no active worker to ask";
      return;
    }
    active.postMessage({ type: "trace-show-real" });
    lines = "asked the worker for a real-shaped one: click it, then Refresh";
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
    <button class="btn btn-secondary" onclick={() => void test()}>Test</button>
    <button class="btn btn-secondary" onclick={() => void testLater()}
      >Test in 5s</button
    >
    <button class="btn btn-secondary" onclick={() => void workerTest(false)}
      >Worker</button
    >
    <button class="btn btn-secondary" onclick={() => void workerTest(true)}
      >Worker + icon</button
    >
    <button class="btn btn-secondary" onclick={() => void realShape()}
      >Real shape</button
    >
    <button class="btn btn-secondary" onclick={() => void clear()}>Clear</button
    >
  </div>
  <pre
    class="text-text-primary overflow-auto text-xs whitespace-pre-wrap">{worker}</pre>
  <pre
    class="text-text-primary overflow-auto text-xs whitespace-pre-wrap">{lines}</pre>
</main>
