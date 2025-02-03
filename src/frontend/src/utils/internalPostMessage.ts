/**
 * Utility methods to reliably and securely establish an internal cross-origin
 * post message communication channel between two related origins.
 */

const READY_INTERVAL = 100;
const READY_TIMEOUT = 3000;

interface ReadyRequest {
  ii_ready_request: string;
}

interface ReadyResponse {
  ii_ready_response: string;
}

const isReadyRequest = (data: unknown): data is ReadyRequest =>
  typeof data === "object" && data !== null && "ii_ready_request" in data;

const isReadyResponse = (data: unknown): data is ReadyResponse =>
  typeof data === "object" && data !== null && "ii_ready_response" in data;

export const waitForWindowReadyResponse = (
  targetWindow: Window,
  targetOrigin: string
): Promise<void> =>
  new Promise<void>((resolve, reject) => {
    const id = window.crypto.randomUUID();
    const listener = (event: MessageEvent) => {
      if (
        event.origin !== targetOrigin ||
        !isReadyResponse(event.data) ||
        event.data.ii_ready_response !== id
      ) {
        return;
      }
      clearInterval(interval);
      clearTimeout(timeout);
      resolve();
    };
    window.addEventListener("message", listener);
    const interval = setInterval(
      () =>
        targetWindow.postMessage(
          {
            ii_ready_request: id,
          } as ReadyRequest,
          targetOrigin
        ),
      READY_INTERVAL
    );
    const timeout = setTimeout(() => {
      clearInterval(interval);
      reject("Ready timed out");
    }, READY_TIMEOUT);
  });

export const waitForWindowReadyRequest = (
  targetWindow: Window,
  targetOrigins: string[]
): Promise<string> =>
  new Promise<string>((resolve) => {
    const listener = (event: MessageEvent) => {
      if (
        !targetOrigins.includes(event.origin) ||
        !isReadyRequest(event.data)
      ) {
        return;
      }
      window.removeEventListener("message", listener);
      targetWindow.postMessage(
        { ii_ready_response: event.data.ii_ready_request } as ReadyResponse,
        event.origin
      );
      resolve(event.origin);
    };
    window.addEventListener("message", listener);
  });
