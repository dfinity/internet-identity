type BeforeInstallPromptEvent = Event & {
  prompt: () => Promise<void>;
  userChoice: Promise<{ outcome: "accepted" | "dismissed" }>;
};

const isBeforeInstallPromptEvent = (e: Event): e is BeforeInstallPromptEvent =>
  "prompt" in e && "userChoice" in e;

let deferredPrompt: BeforeInstallPromptEvent | undefined;

if (typeof window !== "undefined") {
  window.addEventListener("beforeinstallprompt", (e: Event) => {
    if (!isBeforeInstallPromptEvent(e)) return;
    e.preventDefault();
    deferredPrompt = e;
  });
}

export const isStandaloneDisplay = (): boolean =>
  typeof window !== "undefined" &&
  window.matchMedia("(display-mode: standalone)").matches;

export const hasDeferredInstallPrompt = (): boolean =>
  deferredPrompt !== undefined;

export const maybePromptInstall = async (): Promise<void> => {
  if (deferredPrompt === undefined) return;
  if (isStandaloneDisplay()) {
    deferredPrompt = undefined;
    return;
  }
  try {
    await deferredPrompt.prompt();
    await deferredPrompt.userChoice;
  } catch (err) {
    console.warn("PWA install prompt failed:", err);
  } finally {
    deferredPrompt = undefined;
  }
};
