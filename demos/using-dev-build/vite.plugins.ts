import { readFileSync } from "fs";
import { join } from "path";

// npm run dev = local
// npm run build = local
// icp deploy = local
// icp deploy -e ic = ic
const network = process.env.ICP_NETWORK ?? ("local" as const);

export const readCanisterIds = (): Record<string, string> => {
  const canisterIdsJsonFile =
    network === "ic"
      ? join(process.cwd(), ".icp", "data", "mappings", "ic.ids.json")
      : join(process.cwd(), ".icp", "cache", "mappings", "local.ids.json");

  try {
    type Details = {
      ic?: string;
      local?: string;
    };

    const config: Record<string, Details> = JSON.parse(
      readFileSync(canisterIdsJsonFile, "utf-8"),
    );

    return Object.entries(config).reduce((acc, current: [string, Details]) => {
      const [canisterName, canisterDetails] = current;

      return {
        ...acc,
        [`${canisterName.toUpperCase()}_CANISTER_ID`]:
          canisterDetails[network as keyof Details],
      };
    }, {});
  } catch (e) {
    throw Error(`Could not get canister ID from ${canisterIdsJsonFile}: ${e}`);
  }
};
