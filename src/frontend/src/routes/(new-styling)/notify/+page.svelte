<script lang="ts">
  import { onMount } from "svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { actorForIdentity } from "$lib/stores/session-delegation.store";
  import { remapToLegacyDomain } from "$lib/utils/iiConnection";
  import { t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import NotifyRedirectView from "./NotifyRedirectView.svelte";

  type Status = "checking" | "redirecting" | "denied";
  let status = $state<Status>("checking");
  let senderOrigin = $state<string | undefined>(undefined);
  let destination = $state<string | undefined>(undefined);

  /**
   * Parses `raw` into an origin, rejecting anything that isn't `https:`.
   *
   * The scheme check is load-bearing, not defensive tidiness: `javascript:` and
   * `data:` URLs both report their origin as the *string* `"null"`, so without
   * it a crafted link could satisfy the origin-equality check below and then be
   * handed to `location.href`, executing script on II's own origin.
   */
  const parseOrigin = (raw: string | null): string | undefined => {
    if (raw === null || raw.length === 0) return undefined;
    try {
      const url = new URL(raw);
      return url.protocol === "https:" ? url.origin : undefined;
    } catch {
      return undefined;
    }
  };

  /**
   * Whether two origins are the same application.
   *
   * Not string equality, because II records consent against the *effective*
   * origin, which passes through `remapToLegacyDomain` — so a canister served
   * at `<id>.icp0.io` is consented and attributed as `<id>.ic0.app`. A dApp's
   * own deep links naturally use the domain the user is actually browsing, so
   * comparing the two verbatim rejects legitimate links.
   *
   * Normalising both sides is safe rather than loose: the remap only collapses
   * the boundary-node domains for the *same* subdomain, and that subdomain is
   * the canister id. Two different canisters can never normalise to one origin.
   */
  const sameApp = (a: string, b: string): boolean =>
    remapToLegacyDomain(a) === remapToLegacyDomain(b);

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
    if (url.protocol !== "https:") return undefined;
    return sameApp(url.origin, origin) ? url.href : undefined;
  };

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
