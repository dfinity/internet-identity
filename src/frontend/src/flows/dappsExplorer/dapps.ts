// The list of dapps. This is derived from https://github.com/dfinity/portal:
// * Only dapps using II are used
// * All relevant logos are copied to II's assets
// * Some logos are converted to webp
import dappsJson from "./dapps.json";

// Infer the type of an array's elements
type ElementOf<Arr> = Arr extends readonly (infer ElementOf)[]
  ? ElementOf
  : "argument is not an array";

export type DappDescription = ElementOf<typeof dappsJson>;

// The list of dapps we showcase
export const dapps: DappDescription[] = dappsJson.map((dapp) => ({
  ...dapp,
  /* fix up logo path (inherited from dfinity/portal) to match our assets */
  logo: dapp.logo.replace("/img/showcase/", "/icons/"),
}));
