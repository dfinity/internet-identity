// The single executable inline script of the canister's POST /callback landing
// page. The frontend canister embeds this file verbatim
// (`include_str!` in `src/internet_identity_frontend/src/callback.rs`), serves
// it inside `<script>…</script>`, and pins its sha384 in the response CSP, so
// the bytes here are exactly what runs in the browser and what is hashed.
//
// It must stay a self-contained classic script: no `import`/`export`, no
// module syntax — the canister serves it as-is and the hash must be stable.
// Its behaviour is unit-tested in `callback_script.test.ts`, which executes
// this exact source with injected globals.
//
// Routing (keys shared with `openIdRedirectMarkers.ts`):
//   - `ii-openid-return-path`: the frontend navigated this tab to the IdP and
//     wants the payload back at that in-app path (the wizard's same-tab
//     sign-in). Only a same-origin path is honoured.
//   - `ii-openid-authorize-state`: the authorize page's 1-click flow, which
//     returns to `/authorize?flow=openid-resume`.
//   - neither: the popup flow. The payload goes to the tab that opened us,
//     over the `redirect_callback` BroadcastChannel and, when there is one,
//     straight to `window.opener`, and the popup closes.
//
// `window.opener` can't discriminate the flows: in the authorize flow this tab
// is itself a popup opened by the relying party.
(function () {
  var data = JSON.parse(
    document.getElementById("callback-payload").textContent,
  );
  var returnPath = sessionStorage.getItem("ii-openid-return-path");
  if (
    returnPath === null &&
    sessionStorage.getItem("ii-openid-authorize-state") !== null
  ) {
    returnPath = "/authorize?flow=openid-resume";
  }
  // Same-tab flow. Only an in-app path is honoured: it must start with a
  // single "/" and not "//" or "/\" (char code 92 is a backslash), which a
  // browser would treat as a protocol-relative off-origin navigation.
  if (
    typeof returnPath === "string" &&
    returnPath.charAt(0) === "/" &&
    returnPath.charAt(1) !== "/" &&
    returnPath.charCodeAt(1) !== 92
  ) {
    sessionStorage.setItem("ii-openid-callback-data", JSON.stringify(data));
    window.location.replace(returnPath);
  } else {
    // Popup flow: deliver to the tab that opened us and close.
    var channel = new BroadcastChannel("redirect_callback");
    channel.postMessage(data);
    channel.close();
    if (window.opener !== null) {
      try {
        window.opener.postMessage(data, window.location.origin);
      } catch (e) {
        // A cross-origin or closed opener; the channel already carried it.
      }
    }
    window.close();
  }
})();
