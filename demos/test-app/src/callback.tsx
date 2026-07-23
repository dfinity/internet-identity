/**
 * ICRC-167 redirect flow callback page.
 *
 * The homepage navigates here (with its inputs in the query) when the redirect
 * transport is selected. This page is both the flow's initiator and its
 * callback: it runs `signIn` / `requestAttributes` on load, so a fresh visit
 * starts the flow and II's return replays it to completion. It then hands the
 * results back to the homepage in the hash. Generic — it reads/writes only via
 * the shared `redirectFlow` codec.
 */
import { AuthClient } from "@icp-sdk/auth/client";
import {
  decodeInputs,
  encodeResults,
  type RedirectResults,
} from "./redirectFlow";

const toBase64 = (bytes: Uint8Array): string =>
  // @ts-ignore Uint8Array.prototype.toBase64 is supported in all target browsers
  bytes.toBase64();

const fromBase64 = (value: string): Uint8Array =>
  // @ts-ignore Uint8Array.fromBase64 is supported in all target browsers
  Uint8Array.fromBase64(value);

const run = async (): Promise<void> => {
  const query = new URLSearchParams(window.location.search);

  // `identityProvider` / `derivationOrigin` are only used on the FIRST
  // (initiating) load: the return load only replays journaled responses, and
  // the transport keys its journal by the callback URL — not these — so it is
  // fine that the query (and thus these) are gone on the return load.
  const authClient = new AuthClient({
    transport: "redirect",
    identityProvider: query.get("iiUrl") ?? undefined,
    derivationOrigin: query.get("derivationOrigin") ?? undefined,
    idleOptions: { disableIdle: true },
  });

  // Carry the flow inputs across the redirect: read them from the query on the
  // first load, replay the same values on the return load (query stripped). Run
  // it first so the call order is stable across loads.
  const inputs = await authClient.memoize(() =>
    decodeInputs(window.location.search),
  );

  const results: RedirectResults = {};
  try {
    const maxTimeToLive =
      inputs.maxTimeToLive !== undefined
        ? BigInt(inputs.maxTimeToLive)
        : undefined;
    const wantsAttributes =
      inputs.requestAttributes && inputs.attributeKeys.length > 0;

    const [, attributes] = await Promise.all([
      authClient.signIn({ maxTimeToLive }),
      wantsAttributes
        ? authClient.requestAttributes({
            keys: inputs.attributeKeys,
            // The nonce must be stable across the redirect; `requestAttributes`
            // memoizes the callback's result, so producing a random one here is
            // fine — it runs once and replays on the return load.
            nonce: () =>
              Promise.resolve(
                inputs.nonce !== undefined
                  ? fromBase64(inputs.nonce)
                  : crypto.getRandomValues(new Uint8Array(32)),
              ),
          })
        : Promise.resolve(undefined),
    ]);

    if (attributes !== undefined) {
      results.attributes = {
        data: toBase64(attributes.data),
        signature: toBase64(attributes.signature),
      };
    }
  } catch (error) {
    results.error = error instanceof Error ? error.message : String(error);
  }

  // Hand the results back to the homepage (in the hash). The identity itself is
  // recovered there from the persisted session, so only one-shot values ride
  // the URL.
  window.location.assign(`/#${encodeResults(results)}`);
};

void run();
