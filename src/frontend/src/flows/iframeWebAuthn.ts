import { isNullish } from "@dfinity/utils";

export const WEBAUTHN_IFRAME_PATH = "#iframe/webauthn";

export const webAuthnInIframeFlow = (): Promise<never> => {
  window.addEventListener("message", async (event) => {
    // event.source?.postMessage("why does it not work?");
    // window.parent.postMessage("why does it not work?", "*");
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
  if (isNullish(rpId)) {
    throw new Error("RP id is missing");
  }
  const callbackPromise = new Promise<Credential>((resolve) => {
    const call = () =>
      hiddenIframe.contentWindow?.postMessage(
        {
          ...options,
          publicKey,
        },
        `https://${rpId}`
      );
    setTimeout(call, 2000);
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
