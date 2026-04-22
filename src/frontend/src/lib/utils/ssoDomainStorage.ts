/**
 * Per-device map from an OpenID credential `(iss, sub, aud)` triple to the
 * organization domain the user typed on the SSO screen when they linked
 * that credential.
 *
 * Rationale: the backend credential record only carries the provider-issued
 * claims (`iss`, `sub`, `aud` + the JWT metadata). It has no notion of "this
 * credential was reached via SSO discovery starting from `dfinity.org`". So
 * when the access-methods UI renders a credential whose `iss` happens to be
 * e.g. `accounts.google.com`, it looks identical to a credential obtained
 * via the direct "Sign in with Google" button — and the "Google account"
 * label hides the real SSO provenance.
 *
 * We fix the local-render case by stashing the `iss|sub|aud → domain`
 * mapping in localStorage at the moment `linkSsoAccount` succeeds. Nothing
 * durable — it only helps on the device where the credential was linked,
 * and only until localStorage is cleared. For a persistent cross-device
 * solution the discovery domain would need to live on the backend
 * credential metadata.
 */

const STORAGE_KEY = "ii:sso-domain-by-credential";

type CredentialKey = { iss: string; sub: string; aud: string };

const keyFor = ({ iss, sub, aud }: CredentialKey): string =>
  `${iss}|${sub}|${aud}`;

/** Read the full map from localStorage. Returns an empty object on any
 * parse / access failure so the caller can treat "not found" uniformly. */
const readAll = (): Record<string, string> => {
  try {
    const raw = localStorage.getItem(STORAGE_KEY);
    if (raw === null) return {};
    const parsed: unknown = JSON.parse(raw);
    if (typeof parsed !== "object" || parsed === null) return {};
    const out: Record<string, string> = {};
    for (const [k, v] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof v === "string") out[k] = v;
    }
    return out;
  } catch {
    return {};
  }
};

const writeAll = (map: Record<string, string>): void => {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(map));
  } catch {
    // Quota exceeded, disabled, private-mode, etc. — the map is purely an
    // optimization, so silently giving up is fine.
  }
};

/** Remember that this credential was linked via SSO starting from `domain`. */
export const rememberSsoDomainForCredential = (
  credential: CredentialKey,
  domain: string,
): void => {
  const map = readAll();
  map[keyFor(credential)] = domain;
  writeAll(map);
};

/** Look up the SSO discovery domain for a credential, if known on this
 * device. Returns `undefined` when the credential wasn't linked here or
 * the mapping has been cleared. */
export const lookupSsoDomainForCredential = (
  credential: CredentialKey,
): string | undefined => readAll()[keyFor(credential)];

/** Forget the mapping for a specific credential (e.g. on unlink). Safe to
 * call even if no entry exists. */
export const forgetSsoDomainForCredential = (
  credential: CredentialKey,
): void => {
  const map = readAll();
  delete map[keyFor(credential)];
  writeAll(map);
};
