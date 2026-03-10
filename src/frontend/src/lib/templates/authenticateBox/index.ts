import { withLoader } from "$lib/templates/loader";
import {
  AlreadyInProgress,
  ApiError,
  AuthFail,
  Connection,
  InvalidAuthnMethod,
  InvalidCaller,
  LoginSuccess,
  NoRegistrationFlow,
  PossiblyWrongWebAuthnFlow,
  RateLimitExceeded,
  RegisterNoSpace,
  UnexpectedCall,
  UnknownUser,
  WebAuthnFailed,
} from "$lib/utils/iiConnection";
import { LoginEvents, loginFunnel } from "$lib/utils/analytics/loginFunnel";

// A type representing flow errors present in most flows
export type FlowError =
  | AuthFail
  | WebAuthnFailed
  | UnknownUser
  | ApiError
  | InvalidCaller
  | AlreadyInProgress
  | RateLimitExceeded
  | NoRegistrationFlow
  | UnexpectedCall
  | InvalidAuthnMethod
  | RegisterNoSpace;

const loginPasskey = ({
  connection,
  userNumber,
}: {
  connection: Connection;
  userNumber: bigint;
}) => connection.login(userNumber);

// Find and use a passkey, whether PIN or webauthn
const useIdentityFlow = ({
  userNumber,
  loginPasskey,
}: {
  userNumber: bigint;
  loginPasskey: (
    userNumber: bigint,
  ) => Promise<
    | LoginSuccess
    | AuthFail
    | WebAuthnFailed
    | PossiblyWrongWebAuthnFlow
    | UnknownUser
    | ApiError
  >;
}): Promise<
  | (LoginSuccess & {
      newAnchor: boolean;
      authnMethod: "passkey";
    })
  | AuthFail
  | WebAuthnFailed
  | PossiblyWrongWebAuthnFlow
  | UnknownUser
  | ApiError
  | { tag: "canceled" }
> => {
  const doLoginPasskey = async () => {
    const result = await withLoader(() => loginPasskey(userNumber));
    // We need to trigger the success here because later we don't know whether it was a registration or login.
    if (result.kind === "loginSuccess") {
      loginFunnel.trigger(LoginEvents.Success);
    }
    return { newAnchor: false, authnMethod: "passkey", ...result } as const;
  };
  return doLoginPasskey();
};

// Use a passkey, with concrete impl.
export const useIdentity = ({
  userNumber,
  connection,
}: {
  userNumber: bigint;
  connection: Connection;
}) =>
  useIdentityFlow({
    userNumber,
    loginPasskey: (userNumber) => loginPasskey({ connection, userNumber }),
  });
