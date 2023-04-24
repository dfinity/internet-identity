import { promptDeviceAlias } from "../../components/alias";
import { WEBAUTHN_CANCEL_TEMPLATE } from "../../components/displayError";
import {
  apiResultToLoginFlowResult,
  cancel,
  LoginFlowResult,
} from "../../utils/flowResult";
import { Connection } from "../../utils/iiConnection";
import { setAnchorUsed } from "../../utils/userNumber";
import { unknownToString } from "../../utils/utils";
import { isCancel } from "../../utils/webAuthnErrorUtils";
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
    if (isCancel(e)) {
      return {
        tag: "err",
        ...WEBAUTHN_CANCEL_TEMPLATE,
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
