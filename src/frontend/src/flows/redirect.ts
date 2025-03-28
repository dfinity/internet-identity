import { withLoader } from "$src/components/loader";

export const REDIRECT_CALLBACK_PATH = "/callback";

export const callbackFlow = (): Promise<never> => {
  // User was returned here after redirect from a OpenID flow callback,
  // these flows are always handled in a popup and the callback url is
  // returned to the opener window through the PostMessage API.
  window.opener.postMessage(window.location.href, window.location.origin);

  return withLoader(
    () =>
      new Promise<never>((_) => {
        /* halt */
      })
  );
};

export const redirectInPopup = (url: string): Promise<string> => {
  const callbackPromise = new Promise<string>((resolve) => {
    const listener = (event: MessageEvent) => {
      if (
        event.source === redirectWindow &&
        event.origin === window.location.origin &&
        typeof event.data === "string"
      ) {
        window.removeEventListener("message", listener);
        redirectWindow?.close();
        window.focus();
        resolve(event.data);
      }
    };
    window.addEventListener("message", listener);
  });
  const width = 500;
  const height = 600;
  const left = (window.innerWidth - width) / 2 + window.screenX;
  const top = (window.innerHeight - height) / 2 + window.screenY;
  const redirectWindow = window.open(
    url,
    "_blank",
    `width=${width},height=${height},left=${left},top=${top}`
  );
  return callbackPromise;
};
