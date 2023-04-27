// Contains code related to the "build features". Features should be accessed
// from the `features` object below. This file also contains helper functions
// for displaying a banner if features are enabled.
export const features = {
  FETCH_ROOT_KEY: `${import.meta.env.II_FETCH_ROOT_KEY ?? "0"}` === "1",
  DUMMY_AUTH: `${import.meta.env.II_DUMMY_AUTH ?? "0"}` === "1",
  DUMMY_CAPTCHA: `${import.meta.env.II_DUMMY_CAPTCHA ?? "0"}` === "1",
};

export const anyFeatures = (): boolean => {
  return Object.values(features).indexOf(true) >= 0;
};
