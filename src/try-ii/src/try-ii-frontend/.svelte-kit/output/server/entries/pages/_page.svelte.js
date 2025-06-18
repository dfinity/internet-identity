import { G as spread_props, D as pop, A as push, O as spread_attributes, M as clsx, T as store_get, P as escape_html, U as unsubscribe_stores, V as store_set } from "../../chunks/index.js";
import { I as Icon, B as Button, a as ButtonOrAnchor } from "../../chunks/Button.js";
import { Delegation, DelegationChain, DelegationIdentity, Ed25519KeyIdentity } from "@dfinity/identity";
import "@dfinity/principal";
import "@dfinity/agent";
import { w as writable } from "../../chunks/index2.js";
function Arrow_right($$payload, $$props) {
  push();
  let { $$slots, $$events, ...props } = $$props;
  const iconNode = [
    ["path", { "d": "M5 12h14" }],
    ["path", { "d": "m12 5 7 7-7 7" }]
  ];
  Icon($$payload, spread_props([
    { name: "arrow-right" },
    props,
    {
      iconNode,
      children: ($$payload2) => {
        props.children?.($$payload2);
        $$payload2.out += `<!---->`;
      },
      $$slots: { default: true }
    }
  ]));
  pop();
}
function Rotate_ccw($$payload, $$props) {
  push();
  let { $$slots, $$events, ...props } = $$props;
  const iconNode = [
    [
      "path",
      {
        "d": "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"
      }
    ],
    ["path", { "d": "M3 3v5h5" }]
  ];
  Icon($$payload, spread_props([
    { name: "rotate-ccw" },
    props,
    {
      iconNode,
      children: ($$payload2) => {
        props.children?.($$payload2);
        $$payload2.out += `<!---->`;
      },
      $$slots: { default: true }
    }
  ]));
  pop();
}
const authWithII = async ({
  url: url_,
  maxTimeToLive,
  allowPinAuthentication,
  derivationOrigin,
  sessionIdentity,
  autoSelectionPrincipal
}) => {
  const iiUrl = new URL(url_);
  iiUrl.hash = "#authorize";
  const win = window.open(iiUrl, "ii-window");
  if (win === null) {
    throw new Error(`Could not open window for '${iiUrl}'`);
  }
  const evnt = await new Promise((resolve) => {
    const readyHandler = (e) => {
      if (e.origin !== iiUrl.origin) {
        return;
      }
      window.removeEventListener("message", readyHandler);
      resolve(e);
    };
    window.addEventListener("message", readyHandler);
  });
  if (evnt.data.kind !== "authorize-ready") {
    throw new Error("Bad message from II window: " + JSON.stringify(evnt));
  }
  const sessionPublicKey = new Uint8Array(
    sessionIdentity.getPublicKey().toDer()
  );
  const request = {
    kind: "authorize-client",
    sessionPublicKey,
    maxTimeToLive,
    derivationOrigin,
    allowPinAuthentication,
    autoSelectionPrincipal
  };
  win.postMessage(request, iiUrl.origin);
  const response = await new Promise((resolve) => {
    const responseHandler = (e) => {
      if (e.origin !== iiUrl.origin) {
        return;
      }
      window.removeEventListener("message", responseHandler);
      win.close();
      resolve(e);
    };
    window.addEventListener("message", responseHandler);
  });
  const message = response.data;
  if (message.kind !== "authorize-client-success") {
    throw new Error("Bad reply: " + JSON.stringify(message));
  }
  const identity2 = identityFromResponse({
    response: message,
    sessionIdentity
  });
  return { identity: identity2, authnMethod: message.authnMethod };
};
const identityFromResponse = ({
  sessionIdentity,
  response
}) => {
  const delegations = response.delegations.map(extractDelegation);
  const delegationChain = DelegationChain.fromDelegations(
    delegations,
    response.userPublicKey.buffer
  );
  const identity2 = DelegationIdentity.fromDelegation(
    sessionIdentity,
    delegationChain
  );
  return identity2;
};
const extractDelegation = (signedDelegation) => ({
  delegation: new Delegation(
    signedDelegation.delegation.pubkey,
    signedDelegation.delegation.expiration,
    signedDelegation.delegation.targets
  ),
  signature: signedDelegation.signature.buffer
});
let identity = writable(void 0);
let localIdentity = writable(
  Ed25519KeyIdentity.generate()
);
function Badge($$payload, $$props) {
  push();
  const {
    children,
    class: className,
    color = "surface",
    size = "md",
    $$slots,
    $$events,
    ...props
  } = $$props;
  $$payload.out += `<div${spread_attributes(
    {
      ...props,
      class: clsx([
        "rounded-full border font-medium",
        {
          surface: "border-border-tertiary text-text-secondary bg-bg-primary"
        }[color],
        {
          sm: "px-2 py-0.5 text-xs",
          md: "px-2 py-0.5 text-sm",
          lg: "px-2.5 py-1 text-sm"
        }[size],
        className
      ])
    },
    null
  )}>`;
  children?.($$payload);
  $$payload.out += `<!----></div>`;
  pop();
}
function _page($$payload, $$props) {
  push();
  var $$store_subs;
  const handleSignin = async (beta) => {
    try {
      const result = await authWithII({
        url: beta ? "https://beta.id.ai" : "https://id.ai",
        sessionIdentity: store_get($$store_subs ??= {}, "$localIdentity", localIdentity)
      });
      console.log(result);
      store_set(identity, result.identity);
    } catch (e) {
      console.error(e);
    }
  };
  const handleSignOut = () => {
    store_set(identity, void 0);
  };
  $$payload.out += `<div class="flex justify-center items-center flex-col flex-1">`;
  if (store_get($$store_subs ??= {}, "$identity", identity) === void 0) {
    $$payload.out += "<!--[-->";
    $$payload.out += `<div class="flex flex-col gap-6 my-4 items-center flex-1 justify-center py-16"><h1 class="text-text-primary font-medium text-4xl text-center">Internet Identity <span class="text-text-secondary">v2</span></h1> <p class="text-lg text-text-secondary px-8 max-w-160 text-balance text-center mb-2">Experience next-gen decentralized authentication with enhanced features
        that make signing in easier than ever.</p> <div class="flex flex-col gap-2 justify-center items-center">`;
    Button($$payload, {
      onclick: () => handleSignin(),
      size: "xl",
      class: "transition-all",
      children: ($$payload2) => {
        $$payload2.out += `<span>Try the new sign-in experience</span> `;
        Arrow_right($$payload2, { size: "1.25rem" });
        $$payload2.out += `<!---->`;
      },
      $$slots: { default: true }
    });
    $$payload.out += `<!----> `;
    ButtonOrAnchor($$payload, {
      onclick: () => handleSignin(true),
      class: "text-text-secondary box-border flex items-center justify-center justify-self-start whitespace-nowrap opacity-100 gap-2.5 transition-all hover:underline decoration-1 w-fit text-sm",
      children: ($$payload2) => {
        $$payload2.out += `<span>Try beta</span> `;
        Arrow_right($$payload2, { size: "1rem" });
        $$payload2.out += `<!---->`;
      },
      $$slots: { default: true }
    });
    $$payload.out += `<!----></div></div>`;
  } else {
    $$payload.out += "<!--[!-->";
    $$payload.out += `<div class="flex flex-col gap-6 my-4 items-center flex-1 justify-center py-16">`;
    Badge($$payload, {
      size: "sm",
      class: "mx-6 px-4 py-2",
      children: ($$payload2) => {
        $$payload2.out += `<span class="font-bold text-text-primary mr-1">Principal:</span><span>${escape_html(store_get($$store_subs ??= {}, "$identity", identity).getPrincipal().toText())}</span>`;
      },
      $$slots: { default: true }
    });
    $$payload.out += `<!----> <h1 class="text-text-primary font-medium text-4xl text-center">You're In. Seamlessly and Securely</h1> <p class="text-lg text-text-secondary px-8 max-w-160 text-balance text-center mb-2">You just signed in with next-gen decentralized authentication for
        faster, safer access without passwords or friction.</p> `;
    Button($$payload, {
      size: "xl",
      variant: "secondary",
      onclick: handleSignOut,
      children: ($$payload2) => {
        Rotate_ccw($$payload2, { size: "1.25rem" });
        $$payload2.out += `<!----> <span>Sign out to try again</span>`;
      },
      $$slots: { default: true }
    });
    $$payload.out += `<!----></div>`;
  }
  $$payload.out += `<!--]--> <div class="flex gap-8 px-8 md:flex-row flex-col max-w-320 mb-16"><div class="flex-1 rounded-xl border border-border-secondary p-4 bg-bg-secondary flex flex-col items-start">`;
  Badge($$payload, {
    size: "lg",
    class: "mb-3",
    children: ($$payload2) => {
      $$payload2.out += `<!---->Simplified Access`;
    },
    $$slots: { default: true }
  });
  $$payload.out += `<!----> <h2 class="text-xl text-text-primary mb-2 font-medium">Discoverable Passkeys</h2> <p class="text-md text-text-secondary">No more remembering identity numbers. Simply select your name from the
        list thanks to discoverable passkeys.</p></div> <div class="flex-1 rounded-xl border border-border-secondary p-4 bg-bg-secondary flex flex-col items-start">`;
  Badge($$payload, {
    size: "lg",
    class: "mb-3",
    children: ($$payload2) => {
      $$payload2.out += `<!---->Full Control`;
    },
    $$slots: { default: true }
  });
  $$payload.out += `<!----> <h2 class="text-xl text-text-primary mb-2 font-medium">Multiple Accounts</h2> <p class="text-md text-text-secondary">Use a single identity to create and manage multiple accounts on the same
        dapp, giving you more flexibility and control.</p></div> <div class="flex-1 rounded-xl border border-border-secondary p-4 bg-bg-secondary flex flex-col items-start">`;
  Badge($$payload, {
    size: "lg",
    class: "mb-3",
    children: ($$payload2) => {
      $$payload2.out += `<!---->Low Friction`;
    },
    $$slots: { default: true }
  });
  $$payload.out += `<!----> <h2 class="text-xl text-text-primary mb-2 font-medium">Sign in with Google</h2> <p class="text-md text-text-secondary">Easily onboard web2 users by securely connecting their Google accounts,
        combining familiar access with the power of decentralized identity.</p></div></div></div>`;
  if ($$store_subs) unsubscribe_stores($$store_subs);
  pop();
}
export {
  _page as default
};
