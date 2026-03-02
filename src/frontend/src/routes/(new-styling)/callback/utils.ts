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
    // Monitor popup closure since cross-origin windows prevent close event
    // listening. We periodically check the closed attribute to detect when
    // the user closes the popup, allowing us to reject the promise instead
    // of waiting indefinitely for a result that will never arrive.
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
