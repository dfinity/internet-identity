import { TemplateResult } from "lit-html";
export type Authenticator = {
  alias: string;
  remove?: () => void;
  warn?: TemplateResult;
};

export type Protection =
  | { isProtected: true; unprotect: () => void }
  | { isProtected: false; protect: () => void };

export type RecoveryPhrase = {
  reset: () => void;
} & Protection;

export type RecoveryKey = {
  remove: () => void;
};

export type Devices = {
  authenticators: Authenticator[];
  recoveries: {
    recoveryKey?: RecoveryKey;
    recoveryPhrase?: RecoveryPhrase;
  };
};
