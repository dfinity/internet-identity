import { infoScreenTemplate } from "$src/components/infoScreen";
import { I18n } from "$src/i18n";
import { renderPage } from "$src/utils/lit-html";
import { TemplateResult } from "lit-html";
import copyJson from "./replaceCurrentDeviceOrigin.json";

const replaceCurrentDeviceOriginTemplate = ({
  replace,
  skip,
  i18n,
}: {
  replace: () => void;
  skip: () => void;
  i18n: I18n;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  return infoScreenTemplate({
    cancel: skip,
    cancelText: copy.skip,
    next: replace,
    nextText: copy.replace_origin,
    title: copy.title,
    paragraph: copy.paragraph,
    scrollToTop: true,
    icon: "info",
    pageId: "replace-current-device-origin",
    label: copy.label_icon,
  });
};

export const replaceCurrentDeviceOriginPage = renderPage(
  replaceCurrentDeviceOriginTemplate
);

// Prompt the user to change the origin of the current device.
// Changing the origin improves the UX of the user when logging in with a different device.
export const replaceCurrentDeviceOrigin = (): Promise<{
  action: "skip" | "replace-origin-device";
}> => {
  return new Promise((resolve) =>
    replaceCurrentDeviceOriginPage({
      i18n: new I18n(),
      replace: () => resolve({ action: "replace-origin-device" }),
      skip: () => resolve({ action: "skip" }),
    })
  );
};
