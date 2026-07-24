<script lang="ts">
  import { onMount } from "svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { actorForIdentity } from "$lib/stores/session-delegation.store";
  import { t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import NotifyRedirectView from "./NotifyRedirectView.svelte";

  type Status = "checking" | "redirecting" | "denied";
  let status = $state<Status>("checking");
  let senderOrigin = $state<string | undefined>(undefined);
  let destination = $state<string | undefined>(undefined);

  const parseOrigin = (raw: string | null): string | undefined => {
    if (raw === null || raw.length === 0) return undefined;
    try {
      return new URL(raw).origin;
    } catch {
      return undefined;
    }
  };

  const resolveDestination = (
    origin: string,
    raw: string | null,
  ): string | undefined => {
    if (raw === null || raw.length === 0) return origin;
    let url: URL;
    try {
      url = new URL(raw);
    } catch {
      return undefined;
    }
    return url.origin === origin ? url.href : undefined;
  };

  const isConsentedOrigin = async (origin: string): Promise<boolean> => {
    const identities = Object.values($lastUsedIdentitiesStore.identities);
    for (const identity of identities) {
      const actor = await actorForIdentity(identity.identityNumber);
      if (actor === undefined) continue;
      try {
        const origins = await actor.push_list_consented_origins(
          identity.identityNumber,
        );
        if (origins.includes(origin)) return true;
      } catch {
        continue;
      }
    }
    return false;
  };

  onMount(() => {
    void (async () => {
      const params = new URL(window.location.href).searchParams;
      const origin = parseOrigin(params.get("origin"));
      if (origin === undefined) {
        status = "denied";
        return;
      }
      const target = resolveDestination(origin, params.get("to"));
      if (target === undefined || !(await isConsentedOrigin(origin))) {
        status = "denied";
        return;
      }
      senderOrigin = origin;
      destination = target;
      status = "redirecting";
    })();
  });

  $effect(() => {
    if (status !== "redirecting" || destination === undefined) return;
    const dest = destination;
    const timer = setTimeout(() => {
      window.location.href = dest;
    }, 2500);
    return () => clearTimeout(timer);
  });
</script>

{#if status === "checking"}
  <div class="flex min-h-[100dvh] flex-col items-center justify-center">
    <ProgressRing class="text-fg-tertiary size-8" />
  </div>
{:else if status === "redirecting" && senderOrigin !== undefined}
  <NotifyRedirectView origin={senderOrigin} />
{:else}
  <div
    class="flex min-h-[100dvh] flex-col items-center justify-center px-8 text-center"
  >
    <h1 class="text-text-primary mb-2 text-2xl font-medium">
      {$t`Nothing to open`}
    </h1>
    <p class="text-text-tertiary max-w-sm text-base">
      {$t`This notification link couldn't be verified. Open the app directly instead.`}
    </p>
  </div>
{/if}
