import { browser } from "$app/environment";
import initializedFeatureFlags from "$lib/utils/featureFlags";

export const ssr = false;
export const prerender = true;

export const load = async ({ params }) => {
  if (browser) {
    // console.log(DISCOVERABLE_PASSKEY_FLOW);
    console.log(window.localStorage);
    console.log("wargle");
    // console.log(initializedFeatureFlags);
  }
};
