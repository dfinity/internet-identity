import { toast } from "../../components/toast";

// The list of dapps. This is derived from https://github.com/dfinity/portal:
// * Only dapps using II are used
// * All relevant logos are copied to II's assets
// * Some logos are converted to webp
import type dappsJson from "./dapps.json";

// Infer the type of an array's elements
type ElementOf<Arr> = Arr extends readonly (infer ElementOf)[]
  ? ElementOf
  : "argument is not an array";

export type DappDescription = ElementOf<typeof dappsJson>;

// The list of dapps we showcase
export const getDapps = async (): Promise<DappDescription[]> => {
  const dapps: typeof dappsJson | null = (
    await import(/* webpackChunkName: "dapps" */ "./dapps.json")
  ).default;

  if (dapps === null) {
    toast.error("Could not load dapps list");
    console.error("Could not load dapps module");
    return [];
  }

  return dapps.map((dapp) => ({
    ...dapp,
    /* fix up logo path (inherited from dfinity/portal) to match our assets */
    logo: dapp.logo.replace("/img/showcase/", "/icons/"),
  }));
};
