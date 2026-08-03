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
  decodeSnapshot,
  encodeResults,
  inputsFromSnapshot,
  safeNextPath,
  type RedirectResults,
} from "./redirectFlow";

const toBase64 = (bytes: Uint8Array): string =>
  // @ts-ignore Uint8Array.prototype.toBase64 is supported in all target browsers
  bytes.toBase64();

const fromBase64 = (value: string): Uint8Array =>
  // @ts-ignore Uint8Array.fromBase64 is supported in all target browsers
  Uint8Array.fromBase64(value);

const run = async (): Promise<void> => {
  // Decode the form snapshot from the query synchronously, for the AuthClient
  // constructor. `identityProvider` / `derivationOrigin` are only used on the
  // FIRST (initiating) load; on the return load the query is gone, so these fall
  // back to defaults and the signer/AuthClient journals restore the real values.
  const queryInputs = inputsFromSnapshot(
    decodeSnapshot(window.location.search),
  );
  const authClient = new AuthClient({
    transport: "redirect",
    identityProvider: queryInputs.iiUrl !== "" ? queryInputs.iiUrl : undefined,
    derivationOrigin: queryInputs.derivationOrigin,
    idleOptions: { disableIdle: true },
  });

  // Journal the whole form snapshot so it stays stable across the redirect and
  // can be echoed back to restore the homepage form. Run it first so the call
  // order is stable across loads. The flow inputs are derived from it.
  const form = await authClient.memoize(() =>
    decodeSnapshot(window.location.search),
  );
  const inputs = inputsFromSnapshot(form);

  // Echo the (journaled) snapshot back so the homepage can restore every option;
  // the window navigated away, so it lost them.
  const results: RedirectResults = { form };
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

  // With `next` set, this was the guarded-route flow: return the user to the
  // page they were trying to reach rather than the homepage. Re-narrowed from
  // the journaled snapshot, not from the live URL — `inputs` came through
  // `memoize`, but the check is cheap and keeps the guarantee local.
  const next = safeNextPath(inputs.next);
  if (next !== undefined && results.error === undefined) {
    window.location.assign(next);
    return;
  }

  // Hand the results back to the homepage (in the hash). The identity itself is
  // recovered there from the persisted session, so only one-shot values ride
  // the URL.
  window.location.assign(`/#${encodeResults(results)}`);
};

void run();
