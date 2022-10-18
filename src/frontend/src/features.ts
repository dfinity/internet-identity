// Contains code related to the "build features". Features should be accessed
// from the `features` object below. This file also contains helper functions
// for displaying a banner if features are enabled.
export const features = {
  FETCH_ROOT_KEY: process.env.II_FETCH_ROOT_KEY === "1",
  DUMMY_AUTH: process.env.II_DUMMY_AUTH === "1",
  DUMMY_CAPTCHA: process.env.II_DUMMY_CAPTCHA === "1",
};

export const anyFeatures = (): boolean => {
  return Object.values(features).indexOf(true) >= 0;
};
