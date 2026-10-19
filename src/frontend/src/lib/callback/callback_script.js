// Served verbatim by the canister's POST /callback landing page (embedded with
// include_str! in src/internet_identity_frontend/src/callback.rs) and pinned by
// its sha384 in the response CSP, so these exact bytes are what the browser
// runs and what is hashed. It must stay a self-contained classic script: no
// module syntax, and never a literal closing-script-tag sequence, which would
// end the element early and break the hash. Behaviour is unit-tested in
// callback_script.test.ts.
//
// Routing (keys shared with openIdRedirectMarkers.ts):
//   - ii-openid-return-path: the frontend wants the payload back at that in-app
//     path (the wizard's same-tab sign-in). Only a same-origin path is honoured.
//   - ii-openid-authorize-state: the authorize page's 1-click flow, which
//     returns to /authorize?flow=openid-resume.
//   - neither: the popup flow. The payload goes to the opener over the
//     redirect_callback BroadcastChannel and, when there is one, straight to
//     window.opener, and the popup closes.
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
