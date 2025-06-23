import { authnPages } from "$lib/templates/authenticateBox";
import { toast } from "$lib/templates/toast";
import { authnTemplateAuthorize } from "$lib/legacy/flows/authorize";
import { authnTemplateManage } from "$lib/legacy/flows/manage";
import { dapps } from "./constants";
import { i18n } from "./i18n";

/* Various values used for showcasing both authz & manage authentication flows */

const authnCnfg = {
  register: () => toast.info("Register requested"),
  addDevice: () => toast.info("Add device requested"),
  recover: () => toast.info("Recover requested"),
  onSubmit: (anchor: bigint) => toast.info(`Submitting anchor ${anchor}`),
};

const authzTemplates = authnTemplateAuthorize({
  origin: "https://nowhere.com",
  i18n,
});
const authzTemplatesAlt = authnTemplateAuthorize({
  origin: "https://nowhere.com",
  derivationOrigin: "http://fgte5-ciaaa-aaaad-aaatq-cai.ic0.app",
  i18n,
});

const authzTemplatesKnownAlt = authnTemplateAuthorize({
  origin: "https://nns.ic0.app",
  derivationOrigin: "http://fgte8-ciaaa-aaaad-aaatq-cai.ic0.app",
  i18n,

  knownDapp: dapps.find((dapp) => dapp.name === "NNS Dapp"),
});

const authzTemplatesKnown = authnTemplateAuthorize({
  origin: "https://oc.app",
  i18n,
  knownDapp: dapps.find((dapp) => dapp.name === "OpenChat"),
});

export const authz = authnPages(i18n, { ...authnCnfg, ...authzTemplates });
export const authzAlt = authnPages(i18n, {
  ...authnCnfg,
  ...authzTemplatesAlt,
});
export const authzKnown = authnPages(i18n, {
  ...authnCnfg,
  ...authzTemplatesKnown,
});
export const authzKnownAlt = authnPages(i18n, {
  ...authnCnfg,
  ...authzTemplatesKnownAlt,
});

export const manageTemplates = authnTemplateManage({ dapps });
export const manage = authnPages(i18n, { ...authnCnfg, ...manageTemplates });
