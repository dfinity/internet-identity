import { promptUserNumberTemplate } from "$src/components/promptUserNumber";
import { toast } from "$src/components/toast";
import {
  AuthFail,
  Connection,
  LoginSuccess,
  WebAuthnFailed,
} from "$src/utils/iiConnection";
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
}): Promise<LoginSuccess | { tag: "canceled" }> => {
  return new Promise((resolve) => {
    return recoverWithDevicePage({
      next: async (userNumber: bigint) => {
        const result = await attemptRecovery({ userNumber, connection });

        if (result.kind === "tooManyRecovery") {
          toast.error(
            "This identity has more than one recovery devices, which is not expected"
          );
          return;
        }

        if (result.kind === "noRecovery") {
          toast.error("This identity does not have a recovery device");
          return;
        }

        if (result.kind !== "loginSuccess") {
          result satisfies AuthFail | WebAuthnFailed;
          toast.error("Could not authenticate using the device");
          return;
        }

        result satisfies LoginSuccess;
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
}): Promise<
  | LoginSuccess
  | WebAuthnFailed
  | AuthFail
  | { kind: "noRecovery" }
  | { kind: "tooManyRecovery" }
> => {
  const { recovery_credentials: recoveryCredentials } =
    await connection.lookupCredentials(userNumber);

  if (recoveryCredentials.length === 0) {
    return { kind: "noRecovery" };
  }

  if (recoveryCredentials.length > 1) {
    return { kind: "tooManyRecovery" };
  }

  return await connection.fromWebauthnCredentials(userNumber, [
    recoveryCredentials[0],
  ]);
};
