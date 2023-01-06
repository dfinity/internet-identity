/** parse the II_VERSION, see ./scripts/version for more information */
export const versionString = process.env.II_VERSION ?? "";
export const versionList = versionString.split("\n");
export const version = {
  commit: versionList[0],
  release: versionList[1] === "" ? undefined : versionList[1],
  dirty: versionList[2] === "dirty",
};
