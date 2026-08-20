/**
 * Per-origin display metadata (name, description, logo) for apps that sign in
 * with Internet Identity, e.g. shown on the authorize flow screens.
 *
 * The metadata is sourced permissionlessly from the app itself, which serves a
 * `/.well-known/ii-app-metadata` file (see {@link fetchAppMetadata}) on the
 * origin its identity is derived for — its derivation origin when it uses one,
 * and the origin it signs in from otherwise. That is the origin the delegation
 * and the user's accounts are bound to, and it is the single place an app
 * publishes its presentation: its alternative origins, which it has certified
 * as its own, are then presented identically without duplicating the file.
 *
 * The curated dapps list shipped with II is only used as a fallback while apps
 * migrate to the well-known file, and a valid file always replaces the curated
 * entry wholesale — the app owns its own presentation. When neither source has
 * data, consumers fall back to the origin's hostname.
 */
import { writable, type Readable } from "svelte/store";
import { fetchAppMetadata, type AppMetadata } from "$lib/utils/appMetadata";
import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";

export type { AppMetadata } from "$lib/utils/appMetadata";

const storeByOrigin = new Map<string, Readable<AppMetadata>>();

/** Fallback display metadata from the curated dapps list shipped with II.
 *  Tried for each of the given origins in turn: a curated entry lists the
 *  origins an app is known to sign in from, which needn't include the origin
 *  it derives from, so the displayed origin still resolves an app that
 *  hasn't published the well-known file yet. */
const knownDappMetadata = (...origins: string[]): AppMetadata => {
  const dapps = getDapps();
  for (const origin of origins) {
    const dapp = dapps.find((dapp) => dapp.hasOrigin(origin));
    if (dapp !== undefined) {
      return {
        name: dapp.name,
        description: dapp.oneLiner,
        logo: dapp.logoSrc,
      };
    }
  }
  return {};
};

/**
 * Reactive display metadata for the app identified by the given origin.
 *
 * Resolves synchronously to the curated-list fallback (so known dapps never
 * flash an unbranded screen) and updates in place once the app's own
 * `/.well-known/ii-app-metadata` file has been fetched and validated. The
 * fetch runs once per origin per page load; all subscribers share the result.
 *
 * @param origin The origin the app's identity is derived for — its validated
 *   derivation origin when the authorization request carries one, and the
 *   origin it signs in from otherwise. This is where the metadata is fetched
 *   from, and what the store is cached under.
 * @param displayOrigin The origin the calling screen shows to the user, when
 *   that differs from `origin` (i.e. the app uses a derivation origin). Used
 *   only to widen the curated-list fallback, never as a metadata source.
 */
export const getAppMetadataStore = (
  origin: string,
  displayOrigin: string = origin,
): Readable<AppMetadata> => {
  const existing = storeByOrigin.get(origin);
  if (existing !== undefined) {
    return existing;
  }
  const { subscribe, set } = writable<AppMetadata>(
    knownDappMetadata(origin, displayOrigin),
  );
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
