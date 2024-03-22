import { handleLoginFlowResult } from "$src/components/authenticateBox";
import { nonNullish } from "@dfinity/utils";
import { registerTentativeDevice } from "./flows/addDevice/welcomeView/registerTentativeDevice";
import { authFlowAuthorize } from "./flows/authorize";
import { authFlowManage, renderManageWarmup } from "./flows/manage";
import { createSpa } from "./spa";
import { getAddDeviceAnchor } from "./utils/addDeviceLink";

void createSpa(async (connection) => {
  // Figure out if user is trying to add a device. If so, use the anchor from the URL.
  const addDeviceAnchor = getAddDeviceAnchor();
  if (nonNullish(addDeviceAnchor)) {
    const userNumber = addDeviceAnchor;
    // Register this device (tentatively)
    const registerDeviceResult = await registerTentativeDevice(
      addDeviceAnchor,
      connection
    );
    if (registerDeviceResult.tag === "canceled") {
      // Adding a device was canceled, fall back into default flow
      return authFlowManage(connection);
    }
    registerDeviceResult satisfies { tag: "deviceAdded" };

    const renderManage = renderManageWarmup();

    // If user "Click" continue in success page, proceed with authentication
    const result = await connection.login(userNumber);
    const loginData = await handleLoginFlowResult(result);

    // User have successfully signed-in we can jump to manage page
    if (nonNullish(loginData)) {
      return await renderManage({
        userNumber,
        connection: loginData.connection,
      });
    }
  }

  const url = new URL(document.URL);

  // Simple, #-based routing
  if (url.hash === "#authorize") {
    // User was brought here by a dapp for authorization
    return authFlowAuthorize(connection);
  } else {
    // The default flow
    return authFlowManage(connection);
  }
});
