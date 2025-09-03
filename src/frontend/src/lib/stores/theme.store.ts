import { readable } from "svelte/store";

// A readable store that tracks the user's preferred colour scheme (dark or light).
// Updates automatically when the system theme changes.
export const themeStore = readable(false, (set) => {
  if (typeof window === "undefined") return;

  const media = window.matchMedia("(prefers-color-scheme: dark)");
  set(media.matches);

  const update = (e: MediaQueryListEvent) => set(e.matches);
  media.addEventListener("change", update);

  return () => media.removeEventListener("change", update);
});
