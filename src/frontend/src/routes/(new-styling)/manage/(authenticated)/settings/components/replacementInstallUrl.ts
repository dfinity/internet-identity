import { prepareInstallUrl } from "$lib/utils/notifications/installUrl";

/**
 * The install URL for an app the user's device no longer has.
 *
 * A record that says it delivers refuses an ordinary install, which is what keeps a
 * second app off a device that already has one. The probe the caller sent first is what
 * lifts that refusal: an app that is still there answers it, and this one did not.
 */
export const replacementInstallUrl = async (
  identityNumber: bigint,
): Promise<string | undefined> => {
  try {
    return await prepareInstallUrl({ identityNumber });
  } catch (error) {
    console.error(error);
    return undefined;
  }
};
