import type {
  AttributeGroup,
  AvailableAttribute,
} from "$lib/stores/attributeConsent.store";
import { extractScope } from "$lib/stores/channelHandlers/attributes";

export type MergedOption = {
  /** The option the picker shows. For email rows merged with
   *  verified_email this is the verified version; otherwise it's
   *  whatever the canister returned. */
  display: AvailableAttribute;
  /** Every requested attribute that contributed to this option.
   *  Certified together when the user accepts the row, so a single
   *  user click consents to email + verified_email when both were
   *  requested for the same scope. */
  originals: AvailableAttribute[];
};

/** A MergedGroup maps to one AttributePicker row. */
export type MergedGroup = {
  name: string;
  omitScope: boolean;
  options: MergedOption[];
};

/** Stable id keyed off `(name, form, scope)`. Used by the consent view's
 *  selections map and `{#each}` block. The scope alone is enough — distinct
 *  scoped requests for the same name carry different scopes; the unscoped
 *  fan-out is name-only. The id stays stable across email/verified_email
 *  merging since merging swaps the option's `display` but not its scope. */
export const groupId = (group: MergedGroup): string => {
  if (group.omitScope) return `${group.name}:u`;
  const scope = extractScope(group.options[0].display.key) ?? "";
  return `${group.name}:s:${scope}`;
};

// =============================================================================
// Internal pipeline: AttributeGroup[] → Bucket[] → MergedGroup[]
// =============================================================================
//
// Each phase is a small loop over `Bucket[]`. A `Bucket` is a draft row
// whose options are keyed by scope, which makes the email/verified_email
// merge a per-scope Map lookup instead of a nested search.
//
//   1. bucketize                       — one Bucket per requested key
//   2. mergeEmailWithVerifiedEmail     — pair email + verified_email per
//                                        (form, scope), drop email-only
//                                        scopes when the dapp asked for
//                                        verified, leave verified-only
//                                        scopes in the verified row.
//   3. collapseIdenticalUnscopedValues — when an unscoped fan-out has
//                                        the same value across every
//                                        scope, collapse to one option.
//   4. toMergedGroups                  — convert to the public shape.
//
// Phases 2 and 3 mutate the Buckets they receive — fine, since `bucketize`
// builds fresh ones and they aren't shared outside this module.
//
// =============================================================================

type Scope = string | undefined;

type Bucket = {
  name: string;
  omitScope: boolean;
  options: Map<Scope, MergedOption>;
};

/** Phase 1. Build one bucket per request identity:
 *  - Unscoped requests for the same name share a bucket whose options
 *    span every scope the canister returned.
 *  - Scoped requests bucket by their full requested key so distinct
 *    issuers (e.g. `openid:google:name` vs `openid:apple:name`) stay
 *    separate.
 *  Identical duplicate requests collapse here — only the first copy of
 *  any `(key, omitScope)` tuple makes it into `originals`. Dropping the
 *  rest avoids sending duplicate specs to the canister, which rejects
 *  the call with a "duplicate attribute" error otherwise. */
const bucketize = (groups: AttributeGroup[]): Bucket[] => {
  const byId = new Map<string, Bucket>();
  for (const group of groups) {
    if (group.options.length === 0) continue;
    const omitScope = group.options[0].omitScope;
    const id = omitScope ? `u:${group.name}` : `s:${group.options[0].key}`;
    let bucket = byId.get(id);
    if (bucket === undefined) {
      bucket = { name: group.name, omitScope, options: new Map() };
      byId.set(id, bucket);
    }
    for (const option of group.options) {
      const scope = extractScope(option.key);
      const existing = bucket.options.get(scope);
      if (existing !== undefined) {
        const isDuplicate = existing.originals.some(
          (og) => og.key === option.key && og.omitScope === option.omitScope,
        );
        if (!isDuplicate) existing.originals.push(option);
        continue;
      }
      bucket.options.set(scope, { display: option, originals: [option] });
    }
  }
  return [...byId.values()];
};

/** Find the verified_email option (and its bucket) that pairs with an
 *  email row's `(omitScope, scope)`. Linear scan: handful of buckets in
 *  practice, and inlining keeps phase 2 readable. */
const findVerifiedFor = (
  buckets: Bucket[],
  omitScope: boolean,
  scope: Scope,
): { bucket: Bucket; option: MergedOption } | undefined => {
  for (const bucket of buckets) {
    if (bucket.name !== "verified_email") continue;
    if (bucket.omitScope !== omitScope) continue;
    const option = bucket.options.get(scope);
    if (option !== undefined) return { bucket, option };
  }
  return undefined;
};

/** Phase 2. Merge email and verified_email per (form, scope).
 *
 *  When the dapp asked for `verified_email`, the user is implicitly
 *  saying "only verified emails count" — so each email bucket is
 *  filtered to the scopes that ALSO appear in a matching verified
 *  bucket of the same form. Email-only scopes fall away (the dapp
 *  doesn't get an unverified email when it asked for a verified one).
 *  Verified-only scopes stay in their own row so the user can still
 *  consent to (or deny) the verified data the dapp requested.
 *
 *  Examples:
 *  - email(a, b) + verified_email(a)        → Email row: a only.
 *  - email(a, b) + verified_email(a, b)     → Email row: a, b.
 *  - email(a, b) + verified_email()         → Email row: a, b.
 *                                             (verified bucket was
 *                                             empty so resolveAttributeGroups
 *                                             didn't even hand us one.)
 *  - email(b)    + verified_email(a)        → Email row: b. Verified row: a.
 *                                             (No intersection; no merge.)
 *  - openid:google:email + openid:google:verified_email
 *                                           → Email row: 1 option,
 *                                             certifies both. */
const mergeEmailWithVerifiedEmail = (buckets: Bucket[]): Bucket[] => {
  for (const bucket of buckets) {
    if (bucket.name !== "email") continue;
    const merged = new Map<Scope, MergedOption>();
    for (const [scope, emailOpt] of bucket.options) {
      const match = findVerifiedFor(buckets, bucket.omitScope, scope);
      if (match === undefined) continue;
      merged.set(scope, {
        display: match.option.display,
        originals: [...match.option.originals, ...emailOpt.originals],
      });
      match.bucket.options.delete(scope);
    }
    if (merged.size > 0) bucket.options = merged;
  }
  return buckets.filter((b) => b.options.size > 0);
};

/** Phase 3. Collapse an unscoped fan-out whose every option carries the
 *  same displayed value into a single option — picking between identical
 *  values isn't a meaningful choice for the user. We keep the first
 *  option as-is (display + originals) and drop the rest: every dropped
 *  option would have produced the same canister output (same attribute
 *  name + same `omit_scope: true` + same value, just from different
 *  scopes), so certifying them all is wasteful and surprises the user
 *  who only saw — and approved — one row. */
const collapseIdenticalUnscopedValues = (buckets: Bucket[]): Bucket[] => {
  for (const bucket of buckets) {
    if (!bucket.omitScope || bucket.options.size <= 1) continue;
    const options = [...bucket.options.values()];
    const firstValue = options[0].display.displayValue;
    if (!options.every((o) => o.display.displayValue === firstValue)) continue;
    const [firstScope] = bucket.options.keys();
    bucket.options = new Map([[firstScope, options[0]]]);
  }
  return buckets;
};

/** Phase 4. Convert the internal bucket representation to the public
 *  shape the consent view consumes. */
const toMergedGroups = (buckets: Bucket[]): MergedGroup[] =>
  buckets.map((b) => ({
    name: b.name,
    omitScope: b.omitScope,
    options: [...b.options.values()],
  }));

export const mergeGroups = (groups: AttributeGroup[]): MergedGroup[] =>
  toMergedGroups(
    collapseIdenticalUnscopedValues(
      mergeEmailWithVerifiedEmail(bucketize(groups)),
    ),
  );
