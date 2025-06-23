import { mainWindow } from "$lib/templates/mainWindow";
import { toast } from "$lib/templates/toast";
import { I18n } from "$lib/legacy/i18n";
import { mount, renderPage } from "$lib/utils/lit-html";
import { Chan } from "$lib/utils/utils";
import { TemplateResult, html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";

import copyJson from "./abortedCredentials.json";

export type AbortReason =
  | "internal_error"
  | "auth_failed_ii"
  | "auth_failed_issuer"
  | "derivation_origin_issuer_error"
  | "invalid_derivation_origin_issuer"
  | "issuer_api_error"
  | "bad_principal_rp"
  | "no_canister_id"
  | "bad_canister_id"
  | "bad_derivation_origin_rp";

/* A screen telling the user the flow was aborted and giving information
 * on why it was aborted and what they can do about it. */
const abortedCredentialsTemplate = ({
  i18n,
  reason,
  onAcknowledge,
  scrollToTop = false,
}: {
  i18n: I18n;
  reason: AbortReason;
  onAcknowledge: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const didAck = new Chan(false);
  const ack = () => {
    didAck.send(true);
    toast.info(html`${copy.you_may_close}`);
    return onAcknowledge();
  };

  const slot = html`
    <hgroup
      data-page="vc-aborted"
      data-abort-reason=${reason}
      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
    >
      <h1 class="t-title t-title--main">${copy.title}</h1>
    </hgroup>
    <p class="t-paragraph">${copy[`aborted_${reason}`]}</p>

    <button
      data-action="cancel"
      ?disabled=${asyncReplace(didAck)}
      class="l-stack c-button c-button--primary"
      @click="${() => ack()}"
    >
      ${copy.ok}
    </button>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const abortedCredentialsPage = renderPage(abortedCredentialsTemplate);
export const abortedCredentials = ({
  reason,
}: {
  reason: AbortReason;
}): Promise<"aborted"> => {
  return new Promise((resolve) =>
    abortedCredentialsPage({
      i18n: new I18n(),
      reason,
      onAcknowledge: () => resolve("aborted"),
      scrollToTop: true,
    }),
  );
};
