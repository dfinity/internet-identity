import { promptDeviceAlias } from "../../components/alias";
import {
  apiResultToLoginFlowResult,
  cancel,
  LoginFlowResult,
} from "../../utils/flowResult";
import { Connection } from "../../utils/iiConnection";
import { setAnchorUsed } from "../../utils/userNumber";
import { unknownToString } from "../../utils/utils";
import { isCancelOrTimeout } from "../../utils/webAuthnErrorUtils";
import { promptCaptcha } from "./captcha";
import { constructIdentity } from "./construct";
import { displayUserNumber } from "./finish";

/** Registration (anchor creation) flow for new users */
export const register = async ({
  connection,
}: {
  connection: Connection;
}): Promise<LoginFlowResult> => {
  try {
    const alias = await promptDeviceAlias({ title: "Register this device" });
    if (alias === null) {
      return cancel;
    }

    const [captcha, identity] = await Promise.all([
      connection.createChallenge(),
      constructIdentity({}),
    ]);

    const captchaResult = await promptCaptcha({
      connection,
      challenge: Promise.resolve(captcha),
      identity,
      alias,
    });

    if ("tag" in captchaResult) {
      return captchaResult;
    } else {
      const result = apiResultToLoginFlowResult(captchaResult);
      if (result.tag === "ok") {
        await displayUserNumber(result.userNumber);
        setAnchorUsed(result.userNumber);
      }
      return result;
    }
  } catch (e) {
    if (isCancelOrTimeout(e)) {
      return {
        tag: "err",
        title: "Operation Canceled",
        message:
          "The interaction with your security device was canceled or timed out. Please try again.",
      };
    }
    return {
      tag: "err",
      title: "Failed to create anchor",
      message: "An error occurred during anchor creation.",
      detail: unknownToString(e, "unknown error"),
    };
  }
};
