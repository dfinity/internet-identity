import { infoScreenTemplate } from "$src/components/infoScreen";
import { I18n } from "$src/i18n";
import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { renderPage } from "$src/utils/lit-html";
import { TemplateResult } from "lit-html";
import copyJson from "./addCurrentDevice.json";
import { addCurrentDevice } from "./manage/addCurrentDevice";

const addCurrentDeviceTemplate = ({
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

export const addCurrentDevicePage = renderPage(addCurrentDeviceTemplate);

// Prompt the user to add the current device (with the current origin).
// Adding the current device to the current origin improves the UX of the user when they come back to this origin.
export const addCurrentDeviceScreen = (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  return new Promise((resolve) =>
    addCurrentDevicePage({
      i18n: new I18n(),
      add: async () => {
        const existingDevices = await connection.lookupAuthenticators(
          userNumber
        );
        await addCurrentDevice(userNumber, connection, existingDevices);
        resolve();
      },
      skip: () => resolve(),
    })
  );
};
