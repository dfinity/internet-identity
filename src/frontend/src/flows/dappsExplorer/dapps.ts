import { toast } from "$src/components/toast";
import { BASE_URL } from "$src/environment";
import { isNullish } from "@dfinity/utils";

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

// Dynamically load the dapps list
const loadDapps = async (): Promise<DappDescription[] | undefined> => {
  try {
    return (await import("./dapps.json")).default;
  } catch (e) {
    console.error(e);
    return undefined;
  }
};

// The list of dapps we showcase
export const getDapps = async (): Promise<DappDescription[]> => {
  // Load the dapps list
  const dapps = await loadDapps();
  if (isNullish(dapps)) {
    toast.error("Could not load dapps list");
    console.error("Could not load dapps module");
    return [];
  }

  return dapps.map((dapp) => ({
    ...dapp,
    /* fix up logo path (inherited from dfinity/portal) to match our assets */
    logo: dapp.logo.replace("/img/showcase/", BASE_URL + "icons/"),
  }));
};
