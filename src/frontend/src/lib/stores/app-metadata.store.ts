/**
 * Per-origin display metadata (name, description, logo) for apps that sign in
 * with Internet Identity, e.g. shown on the authorize flow screens.
 *
 * The metadata is sourced permissionlessly from the app itself: every origin
 * can serve a `/.well-known/ii-app-metadata` file (see
 * {@link fetchAppMetadata}). The curated dapps list shipped with II is only
 * used as a fallback while apps migrate to the well-known file, and a valid
 * file always replaces the curated entry wholesale — the app owns its own
 * presentation. When neither source has data, consumers fall back to the
 * origin's hostname.
 */
import { writable, type Readable } from "svelte/store";
import { fetchAppMetadata, type AppMetadata } from "$lib/utils/appMetadata";
import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";

export type { AppMetadata } from "$lib/utils/appMetadata";

const storeByOrigin = new Map<string, Readable<AppMetadata>>();

/** Fallback display metadata from the curated dapps list shipped with II. */
const knownDappMetadata = (origin: string): AppMetadata => {
  const dapp = getDapps().find((dapp) => dapp.hasOrigin(origin));
  return dapp === undefined
    ? {}
    : { name: dapp.name, description: dapp.oneLiner, logo: dapp.logoSrc };
};

/**
 * Reactive display metadata for the given origin.
 *
 * Resolves synchronously to the curated-list fallback (so known dapps never
 * flash an unbranded screen) and updates in place once the origin's own
 * `/.well-known/ii-app-metadata` file has been fetched and validated. The
 * fetch runs once per origin per page load; all subscribers share the result.
 *
 * @param origin The origin the calling screen displays to the user (its
 *   hostname badge) — usually the postMessage channel origin, or the
 *   validated derivation origin on screens that display that instead. The
 *   displayed origin and the metadata source must always be the same, so the
 *   hostname the user can verify vouches for the presentation next to it.
 */
export const getAppMetadataStore = (origin: string): Readable<AppMetadata> => {
  const existing = storeByOrigin.get(origin);
  if (existing !== undefined) {
    return existing;
  }
  const { subscribe, set } = writable<AppMetadata>(knownDappMetadata(origin));
  const store = { subscribe };
  storeByOrigin.set(origin, store);
  void fetchAppMetadata(origin).then((metadata) => {
    if (metadata !== undefined) {
      set(metadata);
    }
  });
  return store;
};

/** Test-only: drop all cached per-origin stores so fetches run again. */
export const resetAppMetadataStores = (): void => {
  storeByOrigin.clear();
};
