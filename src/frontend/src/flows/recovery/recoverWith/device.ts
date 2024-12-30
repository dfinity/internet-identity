import { infoToastTemplate } from "$src/components/infoToast";
import infoToastCopy from "$src/components/infoToast/copy.json";
import { promptUserNumberTemplate } from "$src/components/promptUserNumber";
import { toast } from "$src/components/toast";
import { I18n } from "$src/i18n";
import { convertToValidCredentialData } from "$src/utils/credential-devices";
import {
  AuthFail,
  Connection,
  LoginSuccess,
  PossiblyWrongRPID,
  WebAuthnFailed,
} from "$src/utils/iiConnection";
import { renderPage } from "$src/utils/lit-html";
import { nonNullish } from "@dfinity/utils";

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
          if (result.kind === "possiblyWrongRPID") {
            const i18n = new I18n();
            const copy = i18n.i18n(infoToastCopy);
            toast.info(
              infoToastTemplate({
                title: copy.title_possibly_wrong_rp_id,
                messages: [
                  copy.message_possibly_wrong_rp_id_1,
                  copy.message_possibly_wrong_rp_id_2,
                ],
              })
            );
            return;
          }

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
  | PossiblyWrongRPID
  | AuthFail
  | { kind: "noRecovery" }
  | { kind: "tooManyRecovery" }
> => {
  const devices = await connection.lookupAll(userNumber);

  const recoveryCredentials = devices.filter(
    ({ purpose }) => "recovery" in purpose
  );

  if (recoveryCredentials.length === 0) {
    return { kind: "noRecovery" };
  }

  if (recoveryCredentials.length > 1) {
    return { kind: "tooManyRecovery" };
  }

  const credentialData = recoveryCredentials
    .map(convertToValidCredentialData)
    .filter(nonNullish);

  return await connection.fromWebauthnCredentials(userNumber, credentialData);
};
