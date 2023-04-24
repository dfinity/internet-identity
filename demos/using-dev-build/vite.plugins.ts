import { readFileSync } from "fs";
import { join } from "path";

export const readCanisterIds = ({
  prefix,
  network,
}: {
  prefix?: string;
  network: string | "ic" | "local";
}): Record<string, string> => {
  const canisterIdsJsonFile =
    network === "ic"
      ? join(process.cwd(), "canister_ids.json")
      : join(process.cwd(), ".dfx", "local", "canister_ids.json");

  try {
    type Details = {
      ic?: string;
      local?: string;
    };

    const config: Record<string, Details> = JSON.parse(
      readFileSync(canisterIdsJsonFile, "utf-8")
    );

    return Object.entries(config).reduce((acc, current: [string, Details]) => {
      const [canisterName, canisterDetails] = current;

      return {
        ...acc,
        [`${prefix ?? ""}${canisterName.toUpperCase()}_CANISTER_ID`]:
          canisterDetails[network as keyof Details],
      };
    }, {});
  } catch (e) {
    throw Error(`Could not get canister ID from ${canisterIdsJsonFile}: ${e}`);
  }
};
