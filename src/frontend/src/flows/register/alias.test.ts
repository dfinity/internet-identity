import { promptDeviceAliasPage } from "./alias";

test("can be canceled", async () => {
  const cancel = jest.fn();
  const ctn = jest.fn((str) => {});
  promptDeviceAliasPage({
    continue: ctn,
    cancel,
    container: document.body,
  });

  const elem = document.querySelector("#registerCancel") as HTMLElement;
  elem.click();

  expect(cancel.mock.calls.length).toBe(1);
});

test("can be picked", async () => {
  const cancel = jest.fn();
  const ctn = jest.fn((str) => {});
  promptDeviceAliasPage({
    continue: ctn,
    cancel,
    container: document.body,
  });

  const input = document.querySelector("#registerAlias") as HTMLInputElement;
  input.value = "foo";

  const button = document.querySelector("#registerButton") as HTMLInputElement;
  button.click();

  expect(ctn.mock.calls.length).toBe(1);
  expect(ctn.mock.calls[0][0]).toBe("foo");
});
