<script lang="ts">
  import { onMount } from "svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { actorForIdentity } from "$lib/stores/session-delegation.store";
  import { parseOrigin, resolveDestination, sameApp } from "./urls";
  import { t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import NotifyRedirectView from "./NotifyRedirectView.svelte";

  type Status = "checking" | "redirecting" | "denied";
  let status = $state<Status>("checking");
  let senderOrigin = $state<string | undefined>(undefined);
  let destination = $state<string | undefined>(undefined);

  // This page fails closed on anything it can't verify, which makes a genuine
  // misconfiguration indistinguishable from an attacker's hand-crafted link.
  // The reason goes to the console rather than the screen: the developer gets
  // what they need to debug, while a crafted link learns nothing it could use
  // to probe which origins an anchor has consented to.
  const deny = (reason: string, detail?: unknown): void => {
    console.warn(`[ii-notify] refused to open: ${reason}`, detail ?? "");
    status = "denied";
  };

  const isConsentedOrigin = async (origin: string): Promise<boolean> => {
    const identities = Object.values($lastUsedIdentitiesStore.identities);
    if (identities.length === 0) {
      console.warn(
        "[ii-notify] no known identities in this browser profile — nothing to check consent against",
      );
      return false;
    }
    let anyActor = false;
    for (const identity of identities) {
      const actor = await actorForIdentity(identity.identityNumber);
      if (actor === undefined) {
        console.warn(
          `[ii-notify] no usable session for identity ${identity.identityNumber} (missing or expired session delegation)`,
        );
        continue;
      }
      anyActor = true;
      try {
        const origins = await actor.push_list_consented_origins(
          identity.identityNumber,
        );
        console.warn(
          `[ii-notify] identity ${identity.identityNumber} consented origins:`,
          origins,
        );
        if (origins.some((consented) => sameApp(consented, origin)))
          return true;
      } catch (error) {
        console.warn(
          `[ii-notify] consent lookup failed for identity ${identity.identityNumber}`,
          error,
        );
        continue;
      }
    }
    if (!anyActor) {
      console.warn(
        "[ii-notify] no identity had a usable session, so consent could not be checked at all",
      );
    }
    return false;
  };

  onMount(() => {
    void (async () => {
      const params = new URL(window.location.href).searchParams;
      const rawOrigin = params.get("origin");
      const rawTo = params.get("to");
      const origin = parseOrigin(rawOrigin);
      if (origin === undefined) {
        deny("the `origin` parameter is missing or not a valid URL", rawOrigin);
        return;
      }
      const target = resolveDestination(origin, rawTo);
      if (target === undefined) {
        deny(
          `the \`to\` target is not on the sender's origin (${origin})`,
          rawTo,
        );
        return;
      }
      if (!(await isConsentedOrigin(origin))) {
        deny(`no identity in this browser has consented to ${origin}`);
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
