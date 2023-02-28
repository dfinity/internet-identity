import { promptDeviceAliasPage } from "../alias";
import { I18n } from "../../i18n";

test("can be canceled", () => {
  const cancel = jest.fn();
  const ctn = jest.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage({
    title: "Title",
    continue: ctn,
    cancel,
    container: document.body,
    i18n: new I18n(),
  });

  const elem = document.querySelector("#pickAliasCancel") as HTMLElement;
  elem.click();

  expect(cancel.mock.calls.length).toBe(1);
});

test("can be picked", () => {
  const cancel = jest.fn();
  const ctn = jest.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage({
    title: "Title",
    continue: ctn,
    cancel,
    container: document.body,
    i18n: new I18n(),
  });

  const input = document.querySelector("#pickAliasInput") as HTMLInputElement;
  input.value = "foo";

  const button = document.querySelector("#pickAliasSubmit") as HTMLInputElement;
  button.click();

  expect(ctn.mock.calls.length).toBe(1);
  expect(ctn.mock.calls[0][0]).toBe("foo");
});
