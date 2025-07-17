import { infoScreenTemplate } from "$lib/templates/infoScreen";
import { I18n } from "$lib/legacy/i18n";
import { AuthenticatedConnection } from "$lib/utils/iiConnection";
import { renderPage } from "$lib/utils/lit-html";
import { TemplateResult } from "lit-html";
import { addCurrentDevice } from "./manage/addCurrentDevice";
import copyJson from "./registerCurrentDeviceCurrentOrigin.json";

const registerCurrentDeviceCurrentOriginTemplate = ({
  add,
  skip,
  i18n,
}: {
  add: () => void;
  skip: () => void;
  i18n: I18n;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  return infoScreenTemplate({
    cancel: skip,
    cancelText: copy.skip,
    next: add,
    nextText: copy.add,
    title: copy.title,
    paragraph: copy.paragraph,
    scrollToTop: true,
    icon: "info",
    pageId: "add-current-device",
    label: copy.label_icon,
  });
};

export const registerCurrentDeviceCurrentOriginPage = renderPage(
  registerCurrentDeviceCurrentOriginTemplate,
);

// Prompt the user to add the current device (with the current origin).
// Adding the current device to the current origin improves the UX of the user when they come back to this origin.
export const registerCurrentDeviceCurrentOrigin = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
): Promise<void> =>
  new Promise((resolve) =>
    registerCurrentDeviceCurrentOriginPage({
      i18n: new I18n(),
      add: async () => {
        const existingDevices =
          await connection.lookupAuthenticators(userNumber);
        // We pass `undefined` as current origin becase we want to add the current device to the current origin.
        await addCurrentDevice(
          userNumber,
          connection,
          existingDevices,
          undefined,
        );
        resolve();
      },
      skip: () => resolve(),
    }),
  );
