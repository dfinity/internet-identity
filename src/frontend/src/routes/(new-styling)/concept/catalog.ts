/**
 * Every curated dapp as an app Explore and the "Add app" search can offer.
 * Mock data like the rest of the concept: nothing here talks to a canister.
 */
import dappsJson from "$lib/legacy/flows/dappsExplorer/dapps.json" with { type: "json" };
import { KnownDapp } from "$lib/legacy/flows/dappsExplorer/dapps";
import { exploreFeed, featuredApps, type FeaturedApp } from "./mockData";

/** The apps the feed already shows, with the one-liners it stands in where
 *  the curated list has none, so an app reads the same in search. */
const shown = new Map(
  [
    ...featuredApps,
    ...exploreFeed.flatMap((item) =>
      "apps" in item ? item.apps : "app" in item ? [item.app] : [],
    ),
  ].map((app) => [app.origin, app]),
);

const featured = (known: KnownDapp): FeaturedApp => {
  const origin = new URL(known.website).origin;
  return (
    shown.get(origin) ?? {
      origin,
      name: known.name,
      logo: known.logoSrc,
      url: known.website,
      oneLiner: known.oneLiner,
    }
  );
};

/** In the curated list's order. The list names some apps twice, once per
 *  domain they answer on; search shows each once, under its first. */
export const catalog: FeaturedApp[] = dappsJson
  .filter(
    (entry, index) =>
      dappsJson.findIndex(({ name }) => name === entry.name) === index,
  )
  .map((entry) => featured(new KnownDapp(entry)));

/**
 * The host a query names, when it looks like a link: a domain with a dot,
 * with or without its scheme ("myapp.icp0.io", "https://oc.app/chat").
 */
export const asLink = (query: string): string | undefined => {
  const text = query.trim();
  if (!text.includes(".") || /\s/.test(text)) {
    return undefined;
  }
  let url: URL;
  try {
    url = new URL(
      /^[a-z][a-z\d+.-]*:\/\//i.test(text) ? text : `https://${text}`,
    );
  } catch {
    return undefined;
  }
  // Labels on both sides of a dot, and a top level that is a word, so a
  // version number or a sentence's last word doesn't pass for a link.
  const labels = url.hostname.split(".");
  const top = labels[labels.length - 1];
  return (url.protocol === "https:" || url.protocol === "http:") &&
    labels.length > 1 &&
    labels.every((label) => label.length > 0) &&
    /^[a-z][a-z\d-]+$/i.test(top)
    ? url.host
    : undefined;
};

/**
 * Apps whose name or one-liner holds the query, ignoring case: names that
 * start with it first, then names that hold it, then one-liners. A pasted
 * link to a listed app finds that app first.
 */
export const searchCatalog = (query: string): FeaturedApp[] => {
  const text = query.trim().toLowerCase();
  if (text === "") {
    return [];
  }
  const host = asLink(query);
  const rank = (app: FeaturedApp): number | undefined => {
    const name = app.name.toLowerCase();
    if (host !== undefined && app.origin === `https://${host}`) {
      return 0;
    }
    if (name.startsWith(text)) {
      return 1;
    }
    if (name.includes(text)) {
      return 2;
    }
    if (app.oneLiner?.toLowerCase().includes(text) === true) {
      return 3;
    }
    return undefined;
  };
  return catalog
    .flatMap((app) => {
      const order = rank(app);
      return order === undefined ? [] : [{ app, order }];
    })
    .sort((a, b) => a.order - b.order)
    .map(({ app }) => app);
};
