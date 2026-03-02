const BROADCAST_CHANNEL = "redirect_callback";
export const REDIRECT_CALLBACK_PATH = "/callback";

export class PopupClosedError extends Error {}

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

  return new Promise<string>((resolve, reject) => {
    // We need to throw an error when the window is closed, else the page and
    // thus the user will wait indefinitely for a result that never comes.
    //
    // We can't listen to close events since the window is likely cross-origin,
    // so instead we periodically check the closed attribute with an interval.
    const closeInterval = setInterval(() => {
      if (redirectWindow?.closed === true) {
        clearInterval(closeInterval);
        reject(new PopupClosedError());
      }
    }, 500);
    // Listen to the popup, we expect a message with the url of the callback,
    // after receiving it we can close the popup and resolve the promise.
    const channel = new BroadcastChannel(BROADCAST_CHANNEL);
    channel.addEventListener("message", (event) => {
      if (typeof event.data !== "string") {
        return;
      }
      channel.close();
      redirectWindow?.close();
      window.focus();
      resolve(event.data);
    });
  });
};

export const sendUrlToOpener = (): void => {
  const channel = new BroadcastChannel(BROADCAST_CHANNEL);
  channel.postMessage(window.location.href);
  channel.close();
};
