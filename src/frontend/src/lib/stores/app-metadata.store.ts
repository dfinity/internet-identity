/**
 * Per-origin display metadata for apps that sign in with Internet Identity:
 * the name, description and logo shown on the authorize flow screens, and the
 * privacy policy and terms of service linked from the MCP connect screen.
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
 *
 * What a visit resolves is kept, because the file and its logo are two requests
 * and an image decode, and the screen is up long before they finish: a user who
 * has signed in to an app before would otherwise watch the curated logo swap for
 * the app's own every time. The fetch still runs and still has the last word.
 */
import { writable, type Readable } from "svelte/store";
import {
  fetchAppMetadata,
  logoAsObjectUrl,
  type AppMetadata,
} from "$lib/utils/appMetadata";
import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";

export type { AppMetadata } from "$lib/utils/appMetadata";

const storeByOrigin = new Map<string, Readable<AppMetadata>>();

const METADATA_CACHE = createStore("ii-app-metadata", "metadata");

/** An entry is refreshed on every visit, so this only bounds how long an app that
 *  has since dropped its file keeps the branding it last published. */
const MAX_CACHE_AGE_MS = 30 * 24 * 60 * 60 * 1000;

/** A resolved document as it is kept: the logo is the blob itself, which IndexedDB
 *  stores by structured clone, so the bytes stay in the browser's blob store rather
 *  than becoming base64 in the JS heap. The live value's `logo` is a `blob:` URL,
 *  which names an object this page created and means nothing to the next one. */
type CachedMetadata = Omit<AppMetadata, "logo"> & {
  logo?: Blob;
  storedAtMillis: number;
};

/** Metadata is a display nicety and must never break the sign-in flow, so a browser
 *  that refuses IndexedDB — a private window, blocked site data — simply has no
 *  cache. `idb-keyval` throws rather than rejects when it cannot open the database,
 *  so this has to be a `try`, not a `.catch`. */
const readCache = async (
  origin: string,
): Promise<CachedMetadata | undefined> => {
  try {
    return await idbGet<CachedMetadata>(origin, METADATA_CACHE);
  } catch {
    return undefined;
  }
};

const writeCache = async (
  origin: string,
  entry: CachedMetadata,
): Promise<void> => {
  try {
    await idbSet(origin, entry, METADATA_CACHE);
  } catch {
    // Nothing to do and nothing to report: the next visit fetches as this one did.
  }
};

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

  // Raced against the fetch rather than awaited before it: a read that lost is a
  // read of what the fetch has just replaced.
  let fetched = false;
  let cachedLogoUrl: string | undefined;
  void readCache(origin).then((cached) => {
    if (
      fetched ||
      cached === undefined ||
      Date.now() - cached.storedAtMillis > MAX_CACHE_AGE_MS
    ) {
      return;
    }
    const { logo, storedAtMillis: _, ...rest } = cached;
    cachedLogoUrl = logo === undefined ? undefined : URL.createObjectURL(logo);
    set({ ...rest, logo: cachedLogoUrl });
  });

  // Taken as it is addressed, which is the one point the blob exists as a blob;
  // what the store carries from here on is a URL naming it.
  let logo: Blob | undefined;
  const addressLogo = (blob: Blob): Promise<string> => {
    logo = blob;
    return logoAsObjectUrl(blob);
  };
  void fetchAppMetadata(origin, addressLogo).then((metadata) => {
    fetched = true;
    if (metadata === undefined) {
      return;
    }
    set(metadata);
    // The cached URL named the page's own copy of the previous logo and nothing
    // renders it now. Revoking keeps the one-URL-per-origin the live value relies
    // on for never having to revoke its own.
    if (cachedLogoUrl !== undefined) {
      URL.revokeObjectURL(cachedLogoUrl);
      cachedLogoUrl = undefined;
    }
    void writeCache(origin, { ...metadata, logo, storedAtMillis: Date.now() });
  });
  return store;
};

/** Test-only: drop all cached per-origin stores so fetches run again. */
export const resetAppMetadataStores = (): void => {
  storeByOrigin.clear();
};
