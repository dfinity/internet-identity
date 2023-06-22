import { promptUserNumberTemplate } from "$src/components/promptUserNumber";
import { toast } from "$src/components/toast";
import {
  apiResultToLoginFlowResult,
  LoginFlowResult,
} from "$src/utils/flowResult";
import { Connection } from "$src/utils/iiConnection";
import { renderPage } from "$src/utils/lit-html";

export const recoverWithDeviceTemplate = ({
  next,
  cancel,
}: {
  next: (userNumber: bigint) => void;
  cancel: () => void;
}) =>
  promptUserNumberTemplate({
    title: "Use Recovery Device",
    message:
      "Enter your Internet Identity and follow your browser's instructions to use your recovery device.",
    onContinue: (userNumber) => next(userNumber),
    onCancel: () => cancel(),
  });

export const recoverWithDevicePage = renderPage(recoverWithDeviceTemplate);

export const recoverWithDevice = ({
  connection,
}: {
  connection: Connection;
}): Promise<LoginFlowResult> => {
  return new Promise((resolve) => {
    return recoverWithDevicePage({
      next: async (userNumber: bigint) => {
        const result = await attemptRecovery({ userNumber, connection });

        if (result.tag === "err") {
          toast.error([result.title, result.message].join(": "));
          return;
        }

        if (result.tag === "canceled") {
          toast.error("Authentication was canceled");
          return;
        }

        result.tag satisfies "ok";
        return resolve(result);
      },
      cancel: () => resolve({ tag: "canceled" }),
    });
  });
};

const attemptRecovery = async ({
  userNumber,
  connection,
}: {
  userNumber: bigint;
  connection: Connection;
}): Promise<LoginFlowResult> => {
  const { recovery_credentials: recoveryCredentials } =
    await connection.lookupCredentials(userNumber);

  if (recoveryCredentials.length === 0) {
    const title = "No recovery device";
    const message = "This identity does not have a recovery device";
    return { tag: "err", title, message };
  }

  if (recoveryCredentials.length > 1) {
    const title = "Too many recovery devices";
    const message =
      "This identity has more than one recovery devices, which is not expected";
    return { tag: "err", title, message };
  }

  const result = apiResultToLoginFlowResult(
    await connection.fromWebauthnCredentials(userNumber, [
      recoveryCredentials[0],
    ])
  );

  return result;
};
