import { isNullish, nonNullish } from "@dfinity/utils";

export const WEBAUTHN_IFRAME_PATH = "#iframe/webauthn";

export const webAuthnInIframeFlow = (): Promise<never> => {
  console.log("iframe is listening!");
  window.addEventListener("message", async (event) => {
    console.log("Received create credential options", event.data);
    window.focus();
    const credential = (await navigator.credentials.get(
      event.data
    )) as PublicKeyCredential;
    console.log("Created credential public key");
    console.log(
      "event.source",
      event.source,
      event.origin,
      event.source?.postMessage
    );
    // TODO: check if origin is trusted with related origins
    window.parent.postMessage(
      {
        id: credential.id,
        type: credential.type,
        rawId: credential.rawId,
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
        // Exclude getClientExtensionResults()
      },
      "*"
    );
  });
  // window.parent.postMessage("ready", "*");

  return new Promise<never>((_) => {
    /* halt */
  });
};

export const webAuthnInIframe = (
  options: CredentialRequestOptions
): Promise<Credential> => {
  const { rpId, ...publicKey } = options.publicKey ?? {};
  console.log("iframe :D");
  if (isNullish(rpId)) {
    throw new Error("RP id is missing");
  }
  const callbackPromise = new Promise<Credential>((resolve) => {
    const call = () => {
      hiddenIframe.focus();
      return hiddenIframe.contentWindow?.postMessage(
        {
          ...options,
          publicKey,
        },
        "*"
      );
    };
    setTimeout(call, 3000);
    const listener = (event: MessageEvent) => {
      console.log("event pre-check", event);
      if (
        event.source === hiddenIframe.contentWindow &&
        event.origin === `https://${rpId}`
      ) {
        console.log("event", event);
        // if (event.data === "ready") {
        //   hiddenIframe.contentWindow?.postMessage(
        //     {
        //       ...options,
        //       publicKey,
        //     },
        //     `https://${rpId}`
        //   );
        //   return;
        // }
        console.log(event.data);
        // window.removeEventListener("message", listener);
        hiddenIframe.remove();
        resolve({
          ...event.data,
          getClientExtensionResults: () => ({}),
        } as PublicKeyCredential);
      }
    };
    console.log("are we sane?");
    window.addEventListener("message", listener);
  });
  const hiddenIframe = document.createElement("iframe");
  hiddenIframe.width = "500px";
  hiddenIframe.height = "500px";
  hiddenIframe.allow = "publickey-credentials-get";
  document.body.appendChild(hiddenIframe);
  hiddenIframe.src = `https://${rpId}${WEBAUTHN_IFRAME_PATH}`;
  return callbackPromise;
};

interface WebAuthnConditions {
  isMobile: boolean;
  extensionLoaded: boolean;
  rorSupported: boolean;
}

interface WebAuthnAttempt {
  iframe: boolean;
  rpId?: string;
}

const createConditions = (): WebAuthnConditions => ({
  isMobile: false,
  extensionLoaded: false,
  rorSupported: true,
});

const createWebAuthnAttempts = (
  currentOrigin: string,
  rpIds: string[],
  conditions: WebAuthnConditions = createConditions()
): WebAuthnAttempt[] =>
  rpIds
    .map((rpId) =>
      new URL(currentOrigin).hostname !== rpId ? rpId : undefined
    )
    .flatMap((rpId) => [
      conditions.extensionLoaded && nonNullish(rpId)
        ? { iframe: true, rpId }
        : undefined,
      conditions.rorSupported ? { iframe: false, rpId } : undefined,
    ])
    .filter(nonNullish);
