import { Connection } from "../../utils/iiConnection";
import { unknownToString } from "../../utils/utils";
import { confirmRegister } from "./captcha";
import { makeCaptcha } from "./captcha";
import { LoginFlowResult, cancel } from "../../utils/flowResult";
import { constructIdentity } from "./construct";
import { promptDeviceAlias } from "./alias";

/** Registration (anchor creation) flow for new users */
export const register = async ({
  connection,
}: {
  connection: Connection;
}): Promise<LoginFlowResult> => {
  try {
    const alias = await promptDeviceAlias();
    if (alias === null) {
      return cancel;
    }

    const [captcha, identity] = await Promise.all([
      makeCaptcha(connection),
      constructIdentity(),
    ]);

    const result = await confirmRegister(
      connection,
      Promise.resolve(captcha),
      identity,
      alias
    );

    return result;
  } catch (e) {
    return {
      tag: "err",
      title: "Failed to create anchor",
      message: "An error occurred during anchor creation.",
      detail: unknownToString(e, "unknown error"),
    };
  }
};
