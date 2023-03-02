import { Connection } from "../../utils/iiConnection";
import { unknownToString } from "../../utils/utils";
import { setAnchorUsed } from "../../utils/userNumber";
import { promptCaptcha } from "./captcha";
import {
  apiResultToLoginFlowResult,
  LoginFlowResult,
  cancel,
} from "../../utils/flowResult";
import { constructIdentity } from "./construct";
import { promptDeviceAlias } from "../../components/alias";
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
    return {
      tag: "err",
      title: "Failed to create anchor",
      message: "An error occurred during anchor creation.",
      detail: unknownToString(e, "unknown error"),
    };
  }
};
