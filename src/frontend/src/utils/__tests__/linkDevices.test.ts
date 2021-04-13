import linkDevices from "../linkDevices";

// Implements feature requirements for #IC-132
describe("Linking devices for an IC User Number", () => {
  it.todo("should require the user to generate a WebAuthn Identity");

  it.todo(
    "should create and display a link to open on an already-authenticated device"
  );

  it.todo("should parse a link on an already authenticated device");

  it.todo("should call add on canister if link is parsed successfully");

  it.todo(
    "should prompt a login if the linkDevices link is opened on a browser without an active session"
  );

  it.todo(
    "should call add on a canister if the user authenticates successfully"
  );

  it.todo(
    "should prompt the user to create a new identity or link if the user fails to authenticate"
  );
});
