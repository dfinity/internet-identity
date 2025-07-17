import { DynamicKey } from "$lib/legacy/i18n";
import { html } from "lit-html";

export const infoToastTemplate = ({
  title,
  messages,
}: {
  title: string | DynamicKey;
  messages: string[] | DynamicKey[];
}) => html`
  <h3 class="t-title c-card__title">${title}</h3>
  ${messages.map(
    (message) =>
      html`<p data-role="info-message" class="t-paragraph">${message}</p>`,
  )}
`;
