import { Funnel } from "./Funnel";

export const DirectOpenIdEvents = {
  RedirectToOpenId: "redirect-to-openid",
  CallbackFromOpenId: "callback-from-openid",
  RedirectToApp: "redirect-to-app",
} as const;

export const directOpenIdFunnel = new Funnel<typeof DirectOpenIdEvents>(
  "direct-openid",
  true,
);
