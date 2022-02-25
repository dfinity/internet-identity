import { displayError } from "../../components/displayError";
import { IIConnection } from "../../utils/iiConnection";
import { getUserNumber } from "../../utils/userNumber";
import { unknownToString } from "../../utils/utils";
import { loginUnknownAnchor } from "./unknownAnchor";
import { loginKnownAnchor } from "./knownAnchor";
import { LoginFlowResult } from "./flowResult";

// We retry logging in until we get a successful Identity Anchor connection pair
// If we encounter an unexpected error we reload to be safe
export const login = async (): Promise<{
  userNumber: bigint;
  connection: IIConnection;
}> => {
  try {
    const x = await tryLogin();

    switch (x.tag) {
      case "ok": {
        return { userNumber: x.userNumber, connection: x.connection };
      }
      case "err": {
        await displayError({ ...x, primaryButton: "Try again" });
        return login();
      }
    }
  } catch (err: unknown) {
    await displayError({
      title: "Something went wrong",
      message:
        "An unexpected error occurred during authentication. Please try again",
      detail: unknownToString(err, "Unknown error"),
      primaryButton: "Try again",
    });
    window.location.reload();
    return Promise.reject(err);
  }
};

const tryLogin = async (): Promise<LoginFlowResult> => {
  const userNumber = getUserNumber();
  if (userNumber === undefined) {
    return loginUnknownAnchor();
  } else {
    return loginKnownAnchor(userNumber);
  }
};
