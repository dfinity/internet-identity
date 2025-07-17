export const BASE_URL = import.meta.env.BASE_URL;

export const VERSION = import.meta.env.II_VERSION ?? "";

export const FETCH_ROOT_KEY = import.meta.env.II_FETCH_ROOT_KEY === "1";
export const DUMMY_AUTH = import.meta.env.II_DUMMY_AUTH === "1";
export const DUMMY_CAPTCHA = import.meta.env.II_DUMMY_CAPTCHA === "1";
