import { Connection } from "$src/utils/iiConnection";
import {
  waitForWindowReadyRequest,
  waitForWindowReadyResponse,
} from "$src/utils/internalPostMessage";
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
        result: Omit<PublicKeyCredential, "getClientExtensionResults">;
      }
    | {
        error: string;
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
  targetOrigin: string
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
      if ("err" in event.data.ii_credential_response) {
        reject(event.data.ii_credential_response.err);
      }
      window.removeEventListener("message", listener);
    };
    window.addEventListener("message", listener);

    // Request credential
    targetWindow.postMessage(
      {
        ii_credential_request: { id, options },
      } as CredentialRequest,
      targetOrigin
    );
  });

const handleCredentialRequest = (
  targetWindow: Window,
  targetOrigin: string
): void =>
  window.addEventListener("message", async (event: MessageEvent) => {
    if (
      event.source === targetWindow &&
      event.origin === targetOrigin &&
      isCredentialRequest(event.data)
    ) {
      try {
        const credential = (await navigator.credentials.get(
          event.data.ii_credential_request.options
        )) as PublicKeyCredential;
        window.parent.postMessage(
          {
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
                      ? credential.response.authenticatorData
                      : undefined,
                  signature:
                    "signature" in credential.response
                      ? credential.response.signature
                      : undefined,
                  userHandle:
                    "userHandle" in credential.response
                      ? credential.response.userHandle
                      : undefined,
                },
              },
            },
          } as CredentialResponse,
          targetOrigin
        );
      } catch (error) {
        window.parent.postMessage(
          {
            ii_credential_response: {
              id: event.data.ii_credential_request.id,
              error,
            },
          } as CredentialResponse,
          targetOrigin
        );
      }
    }
  });

export const webAuthnInIframeFlow = async (
  connection: Connection
): Promise<never> => {
  // Establish cross-origin connection with parent window
  const config = await connection.getConfig();
  const targetOrigin = await waitForWindowReadyRequest(
    window.parent,
    config.related_origins[0] ?? []
  );

  // Get credential and send to parent window
  handleCredentialRequest(window.parent, targetOrigin);

  return new Promise<never>((_) => {
    /* halt */
  });
};

export const webAuthnInIframe = async (
  options: CredentialRequestOptions
): Promise<Credential> => {
  if (isNullish(options.publicKey?.rpId)) {
    throw new Error("RP id is missing");
  }
  const targetOrigin = `https://${options.publicKey?.rpId}`;

  // Make sure iframe stays focused, the WebAuthn interaction will fail
  // in particularly the Safari browser when focused has been lost.
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
  } finally {
    iframe.remove();
  }
};
