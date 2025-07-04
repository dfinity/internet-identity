import { readable } from "svelte/store";

function createDarkModeStore() {
  // Helper to check dark mode
  const getIsDarkMode = () =>
    typeof window !== "undefined" &&
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)").matches;

  return readable(getIsDarkMode(), (set) => {
    if (typeof window === "undefined" || !window.matchMedia) return;

    const mediaQuery = window.matchMedia("(prefers-color-scheme: dark)");

    // Handler to update the store when the preference changes
    const update = () => set(mediaQuery.matches);

    // Listen for changes
    mediaQuery.addEventListener("change", update);

    // Cleanup
    return () => {
      mediaQuery.removeEventListener("change", update);
    };
  });
}

export const isDarkMode = createDarkModeStore();
