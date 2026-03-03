const BROADCAST_CHANNEL = "redirect_callback";
export const REDIRECT_CALLBACK_PATH = "/callback";

export class CallbackPopupClosedError extends Error {}

export const redirectInPopup = (url: string): Promise<string> => {
  const width = 500;
  const height = 600;
  const left = (window.innerWidth - width) / 2 + window.screenX;
  const top = (window.innerHeight - height) / 2 + window.screenY;
  const redirectWindow = window.open(
    url,
    "_blank",
    `width=${width},height=${height},left=${left},top=${top}`,
  );
  if (redirectWindow === null) {
    throw new CallbackPopupClosedError();
  }

  return new Promise<string>((resolve, reject) => {
    const cleanup = () => {
      clearInterval(closeInterval);
      channel.close();
      redirectWindow?.close();
      window.focus();
    };
    // Periodically check if popup was closed by the user.
    // We can't listen for close events due to cross-origin restrictions,
    // so we poll every 500ms to detect closure. The interval balances
    // responsiveness with resource consumption.
    const closeInterval = setInterval(() => {
      if (redirectWindow.closed === true) {
        cleanup();
        reject(new CallbackPopupClosedError());
      }
    }, 500);
    // Listen to the popup, we expect a message with the url of the callback,
    // after receiving it we can close the popup and resolve the promise.
    const channel = new BroadcastChannel(BROADCAST_CHANNEL);
    channel.addEventListener("message", (event) => {
      if (typeof event.data !== "string") {
        return;
      }
      cleanup();
      resolve(event.data);
    });
  });
};

export const sendUrlToOpener = (): void => {
  const channel = new BroadcastChannel(BROADCAST_CHANNEL);
  channel.postMessage(window.location.href);
  channel.close();
};
