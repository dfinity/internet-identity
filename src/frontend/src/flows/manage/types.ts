import { TemplateResult } from "lit-html";

// A simple authenticator (non-recovery device)
export type Authenticator = {
  alias: string;
  last_usage: [] | [bigint];
  rename: () => void;
  remove?: () => void;
  // `warn` is used to show a warning icon when the device was registered in a different oring than current one.
  warn?: TemplateResult;
  // `info` is used to show an info icon of where the device was registered, only when some device has a different origin than the others.
  info?: TemplateResult;
};

// A recovery phrase, potentially protected
export type RecoveryPhrase = {
  reset: () => void;
} & Protection;

export type Protection =
  | { isProtected: true; unprotect: () => void }
  | { isProtected: false; protect: () => void };

// A recovery key, i.e. "external hardware"
export type RecoveryKey = {
  remove: () => void;
};

// The devices an anchor is expected to have
export type Devices = {
  authenticators: Authenticator[];
  recoveries: {
    recoveryKey?: RecoveryKey;
    recoveryPhrase?: RecoveryPhrase;
  };
  pinAuthenticators: Authenticator[];
};
