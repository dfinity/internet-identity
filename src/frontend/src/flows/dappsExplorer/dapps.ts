import { features } from "$src/features";

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
export const getDapps = (): DappDescription[] => {
  const dapps = [...dappsJson];

  // XXX: Piggy back on the DUMMY_CAPTCHA feature to assume this is a test build
  // and add test data
  if (features.DUMMY_CAPTCHA) {
    // The dapp used in tests
    const nnsDappLogo = dapps.find((dapp) => dapp.name === "NNS Dapp");
    dapps.push({
      name: "Test Dapp",
      website: "https://nice-name.com",
      logo: nnsDappLogo?.logo ?? "no-such-logo",
    });
  }

  return dapps;
};
