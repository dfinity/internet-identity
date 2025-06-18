import { Connection } from "$lib/utils/iiConnection";
import {
  waitForWindowReadyRequest,
  waitForWindowReadyResponse,
} from "$lib/utils/internalPostMessage";
import { isNullish } from "@dfinity/utils";

export const WEBAUTHN_IFRAME_PATH = "#iframe/webauthn";

interface CredentialRequest {
  ii_credential_request: {
    id: string;
    options: CredentialRequestOptions;
  };
}

interface CredentialResponse {
  ii_credential_response: {
    id: string;
  } & (
    | {
        result: Omit<
          PublicKeyCredential,
          "getClientExtensionResults" | "toJSON"
        > & {
          response: {
            authenticatorData?: ArrayBuffer;
            signature?: ArrayBuffer;
            userHandle?: ArrayBuffer;
          };
        };
      }
    // We will create a DOMException from this error object
    | {
        error: {
          name: string;
          message: string;
        };
      }
  );
}

const isCredentialRequest = (data: unknown): data is CredentialRequest =>
  typeof data === "object" && data !== null && "ii_credential_request" in data;

const isCredentialResponse = (data: unknown): data is CredentialResponse =>
  typeof data === "object" && data !== null && "ii_credential_response" in data;

const requestCredential = (
  options: CredentialRequestOptions,
  targetWindow: Window,
  targetOrigin: string,
): Promise<PublicKeyCredential> =>
  new Promise<PublicKeyCredential>((resolve, reject) => {
    // Listen for credential response
    const id = window.crypto.randomUUID();
    const listener = (event: MessageEvent) => {
      if (
        event.source !== targetWindow ||
        event.origin !== targetOrigin ||
        !isCredentialResponse(event.data)
      ) {
        return;
      }
      if ("result" in event.data.ii_credential_response) {
        resolve({
          ...event.data.ii_credential_response.result,
          getClientExtensionResults: () => ({}),
        } as PublicKeyCredential);
      }
      if ("error" in event.data.ii_credential_response) {
        reject(event.data.ii_credential_response.error);
      }
      window.removeEventListener("message", listener);
    };
    window.addEventListener("message", listener);

    // Request credential
    const request: CredentialRequest = {
      ii_credential_request: { id, options },
    };
    targetWindow.postMessage(request, targetOrigin);
  });

const handleCredentialRequest = (
  targetWindow: Window,
  targetOrigin: string,
): void =>
  window.addEventListener("message", async (event: MessageEvent) => {
    if (
      event.source === targetWindow &&
      event.origin === targetOrigin &&
      isCredentialRequest(event.data)
    ) {
      try {
        const credential = (await navigator.credentials.get(
          event.data.ii_credential_request.options,
        )) as PublicKeyCredential;
        const response: CredentialResponse = {
          ii_credential_response: {
            id: event.data.ii_credential_request.id,
            result: {
              // Manually copy values here since credential is not enumerable
              id: credential.id,
              type: credential.type,
              rawId: credential.rawId,
              authenticatorAttachment: credential.authenticatorAttachment,
              response: {
                clientDataJSON: credential.response.clientDataJSON,
                authenticatorData:
                  "authenticatorData" in credential.response
                    ? (credential.response.authenticatorData as ArrayBuffer)
                    : undefined,
                signature:
                  "signature" in credential.response
                    ? (credential.response.signature as ArrayBuffer)
                    : undefined,
                userHandle:
                  "userHandle" in credential.response
                    ? (credential.response.userHandle as ArrayBuffer)
                    : undefined,
              },
            },
          },
        };
        window.parent.postMessage(response, targetOrigin);
      } catch (error: unknown) {
        const response: CredentialResponse = {
          ii_credential_response: {
            id: event.data.ii_credential_request.id,
            // We need to manually copy the error values here since the error instance will be lost in the postMessage
            error: {
              name: "NotAllowedError",
              message: error instanceof Error ? error.message : "Unknown error",
            },
          },
        };
        window.parent.postMessage(response, targetOrigin);
      }
    }
  });

export const webAuthnInIframeFlow = async (
  connection: Connection,
): Promise<never> => {
  // Establish cross-origin connection with parent window
  const targetOrigin = await waitForWindowReadyRequest(
    window.parent,
    // We only establish a connection for the related origins in the II config,
    // incoming requests from other origins are not listed here and ignored.
    //
    // Additionally, the CSP configuration will block any attempt to render II
    // inside an iframe from domains that are not related origins.
    connection.canisterConfig.related_origins[0] ?? [],
  );

  // Get credential and send to parent window
  handleCredentialRequest(window.parent, targetOrigin);

  return new Promise<never>((_) => {
    /* halt */
  });
};

export const webAuthnInIframe = async (
  options: CredentialRequestOptions,
): Promise<Credential> => {
  if (isNullish(options.publicKey?.rpId)) {
    throw new Error("RP id is missing");
  }
  const targetOrigin = `https://${options.publicKey?.rpId}`;

  // WebAuthn fails in Safari if the iframe does not remain focused.
  const iframe = document.body.appendChild(document.createElement("iframe"));
  iframe.style.position = "fixed";
  iframe.style.top = "0";
  iframe.style.left = "0";
  iframe.width = "100%";
  iframe.height = "100%";
  iframe.style.border = "0";
  iframe.style.opacity = "0";
  iframe.style.zIndex = "9999";
  iframe.allow = "publickey-credentials-get";
  iframe.src = `${targetOrigin}${WEBAUTHN_IFRAME_PATH}`;
  iframe.focus();

  try {
    // Wait for iframe to be loaded and ready
    if (isNullish(iframe.contentWindow)) {
      throw new Error("Hidden iframe could not be instantiated");
    }
    await new Promise<void>((resolve, reject) => {
      iframe.onload = () => resolve();
      iframe.onerror = () => reject(new Error("Unable to load hidden iframe"));
    });
    await waitForWindowReadyResponse(iframe.contentWindow, targetOrigin);

    // Request credential from iframe
    return await requestCredential(options, iframe.contentWindow, targetOrigin);
  } catch (e: unknown) {
    // We need to throw a `DOMException` so that it is properly caught by the caller of this function.
    if (typeof e === "object" && e !== null && "name" in e && "message" in e) {
      const message: string =
        typeof e.message === "string" ? e.message : "Unknown error";
      const name: string =
        typeof e.name === "string" ? e.name : "NotAllowedError";
      throw new DOMException(message, name);
    }
    throw new DOMException("NotAllowedError", "Unknown error");
  } finally {
    iframe.remove();
  }
};
