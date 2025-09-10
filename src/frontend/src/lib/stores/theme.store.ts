import { readable } from "svelte/store";
import { browser } from "$app/environment";

// A readable store that tracks the user's preferred colour scheme (dark or light).
// Updates automatically when the system theme changes.
export const themeStore = readable<boolean | null>(null, (set) => {
  if (!browser || typeof window === "undefined") return;

  const media = window.matchMedia("(prefers-color-scheme: dark)");
  const update = () => set(media.matches);

  update(); // sync immediately to avoid flicker
  requestAnimationFrame(update); // backup in case of late browser updates before paint
  media.addEventListener("change", update);

  return () => media.removeEventListener("change", update);
});
