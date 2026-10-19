import { prepareInstallUrl } from "$lib/utils/notifications/installUrl";

/**
 * The install URL for an app the user's device no longer has.
 *
 * A record that says it delivers refuses an ordinary install, which is what keeps a
 * second app off a device that already has one. Only the user can say the app is gone,
 * and the probe above is them saying it, so this is the install that replaces it.
 */
export const replacementInstallUrl = async (
  identityNumber: bigint,
): Promise<string | undefined> => {
  try {
    return await prepareInstallUrl({ identityNumber, replacingExisting: true });
  } catch (error) {
    console.error(error);
    return undefined;
  }
};
