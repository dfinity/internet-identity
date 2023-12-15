import { BASE_URL } from "$src/environment";
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

// A 'known dapp", i.e. a dapp description with the logo fixed up and convenience methods
export class KnownDapp {
  constructor(public descr: DappDescription) {}

  // It's a known dapp if the origin matches
  // * the website
  // * any (some) of the authOrigins
  hasOrigin(orig: string): boolean {
    return (
      orig === new URL(this.descr.website).origin ||
      (this.descr.authOrigins ?? []).some(
        (authOrigin) => orig === new URL(authOrigin).origin
      )
    );
  }

  // Path to use for logo files
  public get logoSrc(): string {
    return BASE_URL + "icons/" + this.descr.logo;
  }

  public get name(): string {
    return this.descr.name;
  }

  public get oneLiner(): string | undefined {
    return this.descr.oneLiner;
  }

  public get website(): string | undefined {
    return this.descr.website;
  }
}

// The list of dapps we showcase
export const getDapps = (): KnownDapp[] => {
  const dapps = [...dappsJson].map((dapp) => new KnownDapp(dapp));

  // XXX: Piggy back on the DUMMY_CAPTCHA feature to assume this is a test build
  // and add test data
  if (features.DUMMY_CAPTCHA) {
    // The dapp used in tests
    const nnsDappLogo = dapps.find((dapp) => dapp.descr.name === "NNS Dapp");
    dapps.push(
      new KnownDapp({
        name: "Test Dapp",
        website: "https://nice-name.com",
        logo: nnsDappLogo?.descr.logo ?? "no-such-logo",
      })
    );
  }

  return dapps;
};
