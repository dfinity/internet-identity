import { readFileSync } from "fs";
import { join } from "path";

export const readCanisterIds = (): Record<string, string> => {
  const canisterIdsJsonFile = join(
    process.cwd(),
    ".dfx",
    "local",
    "canister_ids.json"
  );

  try {
    type Details = {
      local: string;
    };

    const config: Record<string, Details> = JSON.parse(
      readFileSync(canisterIdsJsonFile, "utf-8")
    );

    return Object.entries(config).reduce((acc, current: [string, Details]) => {
      const [canisterName, canisterDetails] = current;

      return {
        ...acc,
        [`${canisterName.toUpperCase()}_CANISTER_ID`]: canisterDetails.local,
      };
    }, {});
  } catch (e) {
    throw Error(`Could not get canister ID from ${canisterIdsJsonFile}: ${e}`);
  }
};
