import { DUMMY_AUTH, DUMMY_CAPTCHA, FETCH_ROOT_KEY } from "$src/environment";

// Contains code related to the "build features". Features should be accessed
// from the `features` object below. This file also contains helper functions
// for displaying a banner if features are enabled.
export const features = {
  FETCH_ROOT_KEY,
  DUMMY_AUTH,
  DUMMY_CAPTCHA,
};

export const anyFeatures = (): boolean => {
  return Object.values(features).indexOf(true) >= 0;
};
