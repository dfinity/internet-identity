import Plausible from "plausible-tracker";

const tracker = Plausible({
  // TODO: Take domain from config
  domain: "identity.internetcomputer.org",
});

export const analytics = {
  pageView: () => {
    tracker.trackPageview();
  },
  event: (name: string, props?: Record<string, string | number | boolean>) => {
    tracker.trackEvent(name, { props });
  },
};
