import { derived, get, writable, type Readable } from "svelte/store";

type BeforeInstallPromptEvent = Event & {
  prompt: () => Promise<void>;
  userChoice: Promise<{ outcome: "accepted" | "dismissed" }>;
};

const isBeforeInstallPromptEvent = (e: Event): e is BeforeInstallPromptEvent =>
  "prompt" in e && "userChoice" in e;

const deferredPrompt = writable<BeforeInstallPromptEvent | undefined>(
  undefined,
);

if (typeof window !== "undefined") {
  window.addEventListener("beforeinstallprompt", (e: Event) => {
    if (!isBeforeInstallPromptEvent(e)) return;
    e.preventDefault();
    deferredPrompt.set(e);
  });
  window.addEventListener("appinstalled", () => {
    deferredPrompt.set(undefined);
  });
}

export const isStandaloneDisplay = (): boolean =>
  typeof window !== "undefined" &&
  window.matchMedia("(display-mode: standalone)").matches;

export const pwaInstallable: Readable<boolean> = derived(
  deferredPrompt,
  ($p) => $p !== undefined,
);

export const promptInstall = async (): Promise<void> => {
  const prompt = get(deferredPrompt);
  if (prompt === undefined) return;
  if (isStandaloneDisplay()) {
    deferredPrompt.set(undefined);
    return;
  }
  try {
    await prompt.prompt();
    await prompt.userChoice;
  } catch (err) {
    console.warn("PWA install prompt failed:", err);
  } finally {
    deferredPrompt.set(undefined);
  }
};
